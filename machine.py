from __future__ import annotations

import logging
import sys

from io_helper import read_file
from isa import Opcode, Operation, read_code


class Memory:
    data: list[Operation | int]

    @staticmethod
    def init(code: list[Operation], limit: int):
        Memory.data = []
        for operation in code:
            if operation.code is Opcode.MEM:
                assert -(2**32) <= operation.arg <= (2**32 - 1), "memory cell is out of bound: {}".format(operation.arg)
                Memory.data.append(operation.arg)
            else:
                Memory.data.append(operation)
        Memory.data.extend([0] * (limit - len(code)))


alu_op_2_operation = {
    Opcode.AND: lambda a, b: int(a and b),
    Opcode.OR: lambda a, b: int(a or b),
    Opcode.EQ: lambda a, b: int(a == b),
    Opcode.NEQ: lambda a, b: int(a != b),
    Opcode.GR: lambda a, b: int(b > a),
    Opcode.LW: lambda a, b: int(b < a),
    Opcode.ADD: lambda a, b: int(a + b),
    Opcode.DIV: lambda a, b: int(b / a),
    Opcode.MOD: lambda a, b: int(b % a),
    Opcode.MUL: lambda a, b: int(a * b),
}


class DataPath:
    def __init__(self, input_buffer: list):
        self.addr_register = 0
        self.acc = 0
        self.op_register = 0
        self.input_buffer = input_buffer
        self.output_buffer = []
        self.buffer_register = 0
        self.signals = {}
        self.memory = Memory

    def latch_addr(self, sel_addr: int | None = None):
        if sel_addr is not None:
            self.addr_register = sel_addr

    def latch_acc(self, sel_acc: Operation | int = None):
        if sel_acc is None:
            self.acc = self.memory.data[self.addr_register]
            return

        if isinstance(sel_acc, int):
            if isinstance(sel_acc, int):
                value = sel_acc
            else:
                value = sel_acc.addr
            assert -(2**32) <= value <= (2**32 - 1), "value is out of bound: {}".format(value)
            self.acc = value
            return

        if sel_acc.code in alu_op_2_operation.keys():
            operation_result = alu_op_2_operation[sel_acc.code](self.acc, self.op_register)
            assert -(2**32) <= operation_result <= (2**32 - 1), "operation result is out of bound: {}".format(
                operation_result
            )
            self.acc = operation_result
            return

        if sel_acc.code == Opcode.IN:
            if len(self.input_buffer) == 0:
                self.acc = 0
                return
            symbol = self.input_buffer.pop(0)
            symbol_code = ord(symbol)
            assert -(2**32) <= symbol_code <= (2**32 - 1), "input token is out of bound: {}".format(symbol_code)
            self.acc = symbol_code
            return

    def latch_op(self):
        self.op_register = self.acc

    def latch_br(self):
        self.buffer_register = self.acc

    def signal_wr(self):
        self.memory.data[self.addr_register] = self.acc

    def signal_output(self, pure: bool = False):
        if pure:
            self.output_buffer.append(str(self.acc))
            return
        symbol = chr(self.acc)
        self.output_buffer.append(symbol)

    def zero(self):
        return self.acc == 0

    def br(self):
        return self.buffer_register

    def acc_val(self):
        return self.acc


class MemoryCorruptedError(Exception):
    pass


class StopMachineError(Exception):
    pass


class ControlUnit:
    def __init__(self, data_path: DataPath):
        self.program_address: int = 0
        self.memory = Memory
        self.data_path: DataPath = data_path
        self.tick_counter: int = 0
        self.counter = 0
        self.instr_counter = 0
        self.buffer_register: int = 0

    def tick(self):
        self.tick_counter += 1

    def signal_latch_program_address(self, operation: Operation = None):
        if operation is not None and operation.code in [Opcode.JP, Opcode.JPZ]:
            self.program_address = operation.arg
        else:
            self.program_address += 1

    def signal_latch_buffer_register(self, val: int):
        self.buffer_register = val

    def decode_and_execute_control_flow_instruction(self, operation: Operation):
        if operation.code == Opcode.HLT:
            raise StopMachineError()

        if operation.code == Opcode.JP or (operation.code == Opcode.JPZ and self.data_path.zero()):
            self.signal_latch_program_address(operation)
            return

        self.signal_latch_program_address()

    @staticmethod
    def translate_pointer(operation: int) -> int | None:
        pointer = operation - (2**31)
        if pointer < 0:
            return None

        return pointer

    def decode_and_execute_instruction(self):  # noqa: C901
        operation = self.memory.data[self.program_address]

        if not isinstance(operation, Operation):
            raise MemoryCorruptedError()

        if operation.code in [Opcode.JP, Opcode.JPZ, Opcode.HLT]:
            self.decode_and_execute_control_flow_instruction(operation)
            self.tick()
            return

        if operation.code in alu_op_2_operation.keys():
            self.data_path.latch_op()
            self.data_path.latch_addr(sel_addr=operation.arg)
            self.tick()

            self.data_path.latch_acc()
            self.tick()

            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()

            return

        if operation.code == Opcode.LD:
            self.data_path.latch_addr(sel_addr=operation.arg)
            self.tick()

            self.data_path.latch_acc()
            self.signal_latch_program_address(operation)
            self.tick()

            return

        if operation.code == Opcode.ST:
            self.data_path.latch_addr(sel_addr=operation.arg)
            self.tick()

            self.data_path.signal_wr()
            self.signal_latch_program_address(operation)
            self.tick()

            return

        if operation.code == Opcode.OUT:
            is_pointer = self.translate_pointer(self.data_path.acc_val())
            if self.counter == 0:
                if is_pointer is None:
                    self.data_path.signal_output()
                    self.signal_latch_program_address(operation)
                    self.tick()
                    return

                self.signal_latch_buffer_register(is_pointer)
                self.data_path.latch_addr(sel_addr=is_pointer)
                self.tick()

                self.data_path.latch_acc()
                self.tick()

                self.data_path.latch_br()
                self.counter += 1
                self.decode_and_execute_instruction()

            if 0 < self.counter <= self.data_path.br():
                self.data_path.latch_addr(sel_addr=(self.buffer_register + self.counter))
                self.tick()

                self.data_path.latch_acc()
                self.tick()

                self.data_path.signal_output()
                self.counter += 1
                self.decode_and_execute_instruction()

            if self.counter != 0:
                self.signal_latch_program_address(operation)
                self.counter = 0
                self.tick()

            return

        if operation.code == Opcode.OUT_PURE:
            self.data_path.signal_output(pure=True)
            self.signal_latch_program_address(operation)
            self.tick()
            return

        if operation.code == Opcode.IN:
            if operation.arg is None:
                self.data_path.latch_acc(sel_acc=operation)
                self.signal_latch_program_address(operation)
                self.tick()
                return

            if self.counter == 0:
                self.data_path.latch_addr(sel_addr=operation.arg)
                self.tick()

                self.data_path.latch_acc()
                self.tick()

                is_pointer = self.translate_pointer(self.data_path.acc_val())
                if is_pointer is not None:
                    self.signal_latch_buffer_register(is_pointer)
                    self.data_path.latch_addr(sel_addr=is_pointer)
                    self.tick()
                else:
                    raise MemoryCorruptedError()

                self.data_path.latch_acc()
                self.data_path.latch_br()
                self.tick()

                self.data_path.latch_acc(sel_acc=operation)
                self.counter += 1
                self.decode_and_execute_instruction()
                self.tick()

            if self.counter >= 1:
                addr = self.buffer_register + self.data_path.br() + self.counter
                if self.data_path.acc_val() != 0:
                    assert addr - self.buffer_register <= 64, "Overflow error"
                    self.data_path.latch_addr(sel_addr=addr)
                    self.tick()
                    self.data_path.signal_wr()
                    self.data_path.latch_acc(sel_acc=operation)
                    self.counter += 1
                    self.decode_and_execute_instruction()
                else:
                    self.data_path.latch_acc(sel_acc=addr - self.buffer_register - 1)
                    self.data_path.latch_addr(sel_addr=self.buffer_register)
                    self.tick()
                    self.data_path.signal_wr()

            if self.counter != 0:
                self.signal_latch_program_address(operation)
                self.counter = 0
                self.tick()

            return

    def log(self, log_settings: LogSettings | None, program_address: int = 0):
        data_path_memory_out = self.memory.data[self.data_path.addr_register]
        if isinstance(data_path_memory_out, Operation):
            data_path_memory_out = data_path_memory_out.code
        state_repr = "INSTR: {:3} TICK: {:3} PC: {:3} ADDR: {:3} MEM_OUT: {} ACC: {} OP_REG: {} COMMAND: {}".format(
            self.instr_counter,
            self.tick_counter,
            self.program_address,
            self.data_path.addr_register,
            data_path_memory_out,
            self.data_path.acc,
            self.data_path.op_register,
            self.memory.data[program_address].to_string(),
        )
        if log_settings is None:
            logging.debug(state_repr)
            return

        if log_settings.verbose_instr >= self.instr_counter:
            logging.debug(state_repr)
        elif log_settings.verbose_instr + 1 == self.instr_counter:
            logging.debug("VERBOSE LOGGING STOPPED...")


class LogSettings:
    def __init__(self, verbose_instr: int):
        self.verbose_instr = verbose_instr


def simulate(
    code: list[Operation],
    input_tokens: list[str],
    memory_size: int,
    instruction_limit: int,
    log_settings: LogSettings | None,
) -> (str, int, int):
    Memory.init(code, memory_size)
    if len(Memory.data) > memory_size:
        logging.critical("Memory limit exceeded")
        return "", 0, 0
    data_path = DataPath(input_tokens)
    control_unit = ControlUnit(data_path)
    try:
        while control_unit.instr_counter < instruction_limit:
            prev_program_address = control_unit.program_address
            control_unit.decode_and_execute_instruction()
            control_unit.log(log_settings, prev_program_address)
            control_unit.instr_counter += 1
    except MemoryCorruptedError:
        logging.critical("Memory corrupted")
        pass
    except StopMachineError:
        pass
    except EOFError:
        logging.warning("Input buffer is empty")
        pass

    if control_unit.instr_counter >= instruction_limit:
        logging.warning("Limit exceeded!")

    logging.debug(f"Total instructions: {control_unit.instr_counter}")
    logging.debug(f"Total ticks: {control_unit.tick_counter}")
    logging.debug(f"Output buffer: {''.join(control_unit.data_path.output_buffer)}")
    logging.debug("Execution stopped")

    return "".join(control_unit.data_path.output_buffer), control_unit.instr_counter, control_unit.tick_counter


def main(code_file: str, input_file: str, log_settings: LogSettings = None):
    code: list[Operation] = read_code(code_file)
    input_tokens = list(read_file(input_file))
    output, instr_counter, tick_counter = simulate(
        code, input_tokens, memory_size=200, instruction_limit=100000, log_settings=log_settings
    )

    print(output)


if __name__ == "__main__":
    assert len(sys.argv) == 3, "Wrong arguments: translator.py <input_file> <target_file>"
    _, source, target = sys.argv
    main(source, target)

from __future__ import annotations

import logging
import sys
from enum import Enum

from io_helper import read_file
from isa import Opcode, Operation, read_code


class DataPathSignals(str, Enum):
    ZERO = "acc is zero"


class Memory:
    data: list[Operation | int]

    @staticmethod
    def init(code: list[Operation]):
        Memory.data = []
        for operation in code:
            if operation.code is Opcode.NOP:
                Memory.data.append(0)
            else:
                Memory.data.append(operation)


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
        self.signals = {}
        self.memory = Memory

    def latch_addr(self, sel_addr: Operation = None):
        if sel_addr is not None:
            self.addr_register = sel_addr.arg

    def latch_acc(self, sel_acc: Operation = None):
        if sel_acc is None:
            self.acc = self.memory.data[self.addr_register]
            return

        if sel_acc.code == Opcode.LIT:
            self.acc = sel_acc.arg
            return

        if sel_acc.code in alu_op_2_operation.keys():
            self.acc = alu_op_2_operation[sel_acc.code](self.acc, self.op_register)
            return

        if sel_acc.code == Opcode.IN:
            if len(self.input_buffer) == 0:
                raise EOFError()
            symbol = self.input_buffer.pop(0)
            symbol_code = ord(symbol)
            assert -128 <= symbol_code <= 127, "input token is out of bound: {}".format(symbol_code)
            self.acc = symbol_code
            return

    def latch_op(self):
        self.op_register = self.acc

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

    def tick(self):
        self.tick_counter += 1

    def signal_latch_program_address(self, operation: Operation = None):
        if operation is not None and operation.code in [Opcode.JP, Opcode.JPZ]:
            self.program_address = operation.arg
        else:
            self.program_address += 1

    def decode_and_execute_control_flow_instruction(self, operation: Operation):
        if operation.code == Opcode.HLT:
            raise StopMachineError()

        if operation.code == Opcode.JP or (operation.code == Opcode.JPZ and self.data_path.zero()):
            self.signal_latch_program_address(operation)
            return

        self.signal_latch_program_address()

    def decode_and_execute_instruction(self):  # noqa: C901
        operation = self.memory.data[self.program_address]

        if not isinstance(operation, Operation):
            raise MemoryCorruptedError()

        if operation.code in [Opcode.JP, Opcode.JPZ, Opcode.HLT]:
            self.decode_and_execute_control_flow_instruction(operation)
            self.tick()

        if operation.code in alu_op_2_operation.keys():
            self.data_path.latch_op()
            self.tick()
            self.data_path.latch_addr(sel_addr=operation)
            self.data_path.latch_acc()
            self.tick()
            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()

        if operation.code == Opcode.LIT:
            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()

        if operation.code == Opcode.LD:
            self.data_path.latch_addr(sel_addr=operation)
            self.data_path.latch_acc()
            self.signal_latch_program_address(operation)
            self.tick()

        if operation.code == Opcode.ST:
            self.data_path.latch_addr(sel_addr=operation)
            self.data_path.signal_wr()
            self.signal_latch_program_address(operation)
            self.tick()

        if operation.code == Opcode.OUT:
            self.data_path.signal_output()
            self.signal_latch_program_address(operation)
            self.tick()
            return

        if operation.code == Opcode.OUT_PURE:
            self.data_path.signal_output(pure=True)
            self.signal_latch_program_address(operation)
            self.tick()

        if operation.code == Opcode.IN:
            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()

    def log(self):
        data_path_memory_out = self.memory.data[self.data_path.addr_register]
        if isinstance(data_path_memory_out, Operation):
            data_path_memory_out = data_path_memory_out.code
        state_repr = "TICK: {:3} PC: {:3} ADDR: {:3} MEM_OUT: {} ACC: {} OP_REG: {} COMMAND: {}".format(
            self.tick_counter,
            self.program_address,
            self.data_path.addr_register,
            data_path_memory_out,
            self.data_path.acc,
            self.data_path.op_register,
            self.memory.data[self.program_address].to_string(),
        )
        logging.debug(state_repr)


def simulate(
    code: list[Operation], input_tokens: list[str], memory_size: int, instruction_limit: int
) -> (str, int, int):
    Memory.init(code)
    if len(Memory.data) > memory_size:
        logging.critical("Memory limit exceeded")
        return "", 0, 0
    data_path = DataPath(input_tokens)
    control_unit = ControlUnit(data_path)
    counter = 0
    try:
        while counter < instruction_limit:
            control_unit.decode_and_execute_instruction()
            control_unit.log()
            counter += 1
    except MemoryCorruptedError:
        logging.critical("Memory corrupted")
        pass
    except StopMachineError:
        pass
    except EOFError:
        logging.warning("Input buffer is empty")
        pass

    if counter >= instruction_limit:
        logging.warning("Limit exceeded!")

    logging.debug(f"Output buffer: {''.join(control_unit.data_path.output_buffer)}")
    logging.debug("Execution stopped")

    return "".join(control_unit.data_path.output_buffer), counter, control_unit.tick_counter


def main(code_file: str, input_file: str):
    code: list[Operation] = read_code(code_file)
    input_tokens = list(read_file(input_file))
    output, instr_counter, tick_counter = simulate(code, input_tokens, memory_size=200, instruction_limit=100000)

    print(output)

    print(" ------------ ФИО ------------- | alg | LoC | code | инстр. | такт. ")
    print(f"Шляпников Александр Дмитриевич | --- | --- | {len(code)} | {instr_counter} | {tick_counter}")


if __name__ == "__main__":
    assert len(sys.argv) == 3, "Wrong arguments: translator.py <input_file> <target_file>"
    _, source, target = sys.argv
    main(source, target)

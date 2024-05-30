from __future__ import annotations

import logging

import click
from io_helper import read_file
from isa import MemoryCell, Opcode, read_code


class MemoryUnit:
    """
    Объект памяти в котором хранятся ячейки с инструкциями и данными
    Инициализируется единожды при старте процессора
    Ссылки на него хранятся в ControlUnit и DataPath
    """

    data: list[MemoryCell | int]

    @staticmethod
    def init(code: list[MemoryCell], limit: int):
        MemoryUnit.data = []
        for operation in code:
            if operation.code is Opcode.MEM:
                assert -(2**32) <= operation.args[0] <= (2**32 - 1), "memory cell is out of bound: {}".format(
                    operation.args[0]
                )
                MemoryUnit.data.append(operation.args[0])
            else:
                MemoryUnit.data.append(operation)
        MemoryUnit.data.extend([0] * (limit - len(code)))


class MemoryCorruptedError(Exception):
    pass


class StopMachineError(Exception):
    pass


alu_op_2_operation = {
    Opcode.AND: lambda a, b: int(a and b),
    Opcode.OR: lambda a, b: int(a or b),
    Opcode.EQ: lambda a, b: int(a == b),
    Opcode.NEQ: lambda a, b: int(a != b),
    Opcode.GR: lambda a, b: int(a > b),
    Opcode.LW: lambda a, b: int(a < b),
    Opcode.ADD: lambda a, b: int(a + b),
    Opcode.SUB: lambda a, b: int(a - b),
    Opcode.DIV: lambda a, b: int(a / b),
    Opcode.MOD: lambda a, b: int(a % b),
    Opcode.MUL: lambda a, b: int(a * b),
}


class IOUnit:
    def __init__(self):
        self.devices = {}

    def register_port(self, port_index: int, value: str | None = None) -> bool:
        if port_index in self.devices:
            return False

        self.devices[port_index] = list(value)
        return True

    def get_value(self, port_index: int) -> int | None:
        if port_index in self.devices and len(self.devices[port_index]) > 0:
            return ord(self.devices[port_index].pop(0))

        return None

    def put_int(self, port_index: int, value: int) -> bool:
        if port_index in self.devices:
            self.devices[port_index].append(str(value))
            return True

        return False

    def put_char(self, port_index: int, value: int) -> bool:
        if port_index in self.devices:
            self.devices[port_index].append(chr(value))
            return True

        return False

    def read_all_from_port(self, port_index: int) -> str:
        if port_index not in self.devices:
            return ""
        return "".join(self.devices[port_index])


class DataPath:
    """
    Класс DataPath
    memory - ссылка на объект Memory память компьютера
    addr_register - регистр текущего адреса для Memory
    acc - аккумулятор
    op_register - регистр для сохранения второго операнда при работе с АЛУ
    input_buffer - буффер имитирующий устройство ввода в процессор
    output_buffer - буффер имитирующий устройство вывода из процессора
    buffer_register - буфферный регистр используется при работе со строками
    """

    def __init__(self, io_unit: IOUnit):
        self.memory = MemoryUnit
        self.io_unit = io_unit
        self.addr_register = 0
        self.acc = 0
        self.op_register = 0
        self.buffer_register = 0

    @staticmethod
    def check_value_valid(value):
        assert -(2**31) <= value <= (2**31 - 1), "value is out of bound: {}".format(value)

    def run_alu(self, sig_op: Opcode, sel_op: int = 1, literal: int = 0) -> int:
        if sel_op == 1:
            operation_result = alu_op_2_operation[sig_op](self.op_register, self.acc)
        elif sel_op == 2:
            operation_result = alu_op_2_operation[sig_op](self.acc, self.buffer_register)
        else:
            operation_result = alu_op_2_operation[sig_op](self.acc, literal)

        self.check_value_valid(operation_result)
        return operation_result

    def latch_addr(self, sel_addr: int):
        """
        Защёлка адресного регистра, может защёлкнуть либо
        * Старое значение +1 (sel_addr = -2)
        * Значение из буферного регистра (sel_addr = -1)
        * Константу (sel_addr >= 0)
        """
        if sel_addr == -2:
            self.addr_register = self.addr_register + 1
        if sel_addr == -1:
            self.addr_register = self.buffer_register
        if sel_addr >= 0:
            self.addr_register = sel_addr

    def latch_acc(self, sel_acc: MemoryCell | int = None, sel_op: int = 1):
        """
        Защёлка аккумулятора, может защёлкнуть либо
        * Значение текущей ячейки памяти
        * Константу
        * Результат операции
        * Значение из устройства ввода
        """
        if sel_acc is None:
            self.acc = self.memory.data[self.addr_register]
            return

        if isinstance(sel_acc, int):
            self.check_value_valid(sel_acc)
            self.acc = sel_acc
            return

        if sel_acc.code in alu_op_2_operation.keys():
            if sel_op == 1:
                self.acc = self.run_alu(sel_acc.code, sel_op=sel_op)
            else:
                self.acc = self.run_alu(sel_acc.code, sel_op=sel_op, literal=sel_op)
            return

        if sel_acc.code == Opcode.IN:
            symbol = self.io_unit.get_value(sel_acc.args[0])
            if symbol is None:
                self.acc = 0
                return
            self.check_value_valid(symbol)
            self.acc = symbol
            return

    def latch_op(self):
        """
        Защёлка регистра операнда
        """
        self.op_register = self.acc

    def latch_br(self, sel_br: int | Opcode, sig_op: Opcode | None = None, sel_op: int = 1):
        """
        Защёлка буферного регистра, может защёлкнуть либо
        * Свое значение -1
        * Значение из аккумулятора
        * Результат операции
        """
        if sel_br == 1:
            self.buffer_register = self.buffer_register - 1
        if sel_br == 2:
            self.buffer_register = self.acc
        if sel_br == 3:
            self.buffer_register = self.run_alu(sig_op, sel_op=sel_op, literal=sel_op)

    def sig_wr(self):
        """
        Сигнал записи в память значения из аккумулятора по текущему адресу
        """
        self.memory.data[self.addr_register] = self.acc

    def sig_out(self, port: int, pure: bool = False):
        """
        Сигнал записи в устройство вывода значения из аккумулятора
        """
        if pure:
            self.io_unit.put_int(port, self.acc)
        else:
            self.io_unit.put_char(port, self.acc)

    def acc_zero(self):
        """
        Сигнал означающий что в аккумуляторе 0
        """
        return self.acc == 0

    def br_zero(self):
        """
        Сигнал означающий что в буфферном регистре 0
        """
        return self.buffer_register == 0

    def br_negative(self):
        """
        Сигнал означающий что в буфферном регистре число < 0
        """
        return self.buffer_register < 0


class ControlUnit:
    def __init__(self, data_path: DataPath):
        self.memory = MemoryUnit
        self.data_path: DataPath = data_path
        self.program_address: int = 0
        self.tick_counter: int = 0
        self.counter: int = 0
        self.instr_counter: int = 0

    def tick(self):
        self.tick_counter += 1

    def signal_latch_program_address(self, operation: MemoryCell = None):
        """
        Защёлка program_address регистра, может защёлкнуть
        * Текущий адрес + 1
        * Адрес из инструкцию управления
        """
        if operation is not None and operation.code in [Opcode.JP, Opcode.JPZ]:
            self.program_address = operation.args[0]
        else:
            self.program_address += 1

    def decode_and_execute_control_flow_instruction(self, operation: MemoryCell):
        """
        Декодирование и исполнение инструкций управления
        HLT - остановка тактового генератора
        JP, JPZ условный и безусловный переходы
        """
        if operation.code == Opcode.HLT:
            raise StopMachineError()

        if operation.code == Opcode.JP or (operation.code == Opcode.JPZ and self.data_path.acc_zero()):
            self.signal_latch_program_address(operation)
            return

        self.signal_latch_program_address()

    def execute_out_instruction(self, operation: MemoryCell):
        """
        Выполняет инструкции для работы с устройством вывода
        OUT_PURE команда отправляет текущее значение аккумулятора в устройство вывода без конвертации в ASCII символ
        OUT команда
         * если текущее значение аккумулятора - указатель, декодирует его и запускает цикл на основе длины строки для
         печати её посимвольно в поток вывода
         * если значение аккумулятора - не указатель, выполняется его отправка в поток вывода с предварительной
         конвертацией в символ ASCII
        """
        if operation.code == Opcode.OUT_PURE:
            self.data_path.sig_out(pure=True, port=operation.args[0])
            self.signal_latch_program_address(operation)
            self.tick()
            return

        self.data_path.latch_br(sel_br=3, sig_op=Opcode.SUB, sel_op=2**31)
        self.tick()

        if self.data_path.br_negative():
            self.data_path.sig_out(port=operation.args[0])
            self.signal_latch_program_address(operation)
            self.tick()
            return

        self.data_path.latch_addr(sel_addr=-1)
        self.tick()

        self.data_path.latch_acc()
        self.tick()

        self.data_path.latch_br(sel_br=2)
        self.data_path.latch_addr(sel_addr=-2)
        self.tick()

        while not self.data_path.br_zero():
            self.data_path.latch_acc()
            self.tick()
            self.data_path.sig_out(operation.args[0])
            self.data_path.latch_addr(sel_addr=-2)
            self.data_path.latch_br(sel_br=1)
            self.tick()

        self.signal_latch_program_address(operation)
        self.tick()

    def execute_in_instruction(self, operation: MemoryCell):
        """
        Выполняет инструкции для работы с устройством ввода
        IN <port> [var] команда
         * если аргумента [var] нет загружает один символ потока ввода в аккумулятор
         * если аргумент [var] передан и это не указатель выкидывается эксепшен
         * если аргумент [var] указатель начинается посимвольное чтение из потока ввода в указанную область памяти
        """
        if len(operation.args) == 1:
            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()
            return

        pointer_addr = operation.args[1]

        self.data_path.latch_addr(sel_addr=pointer_addr)
        self.tick()

        self.data_path.latch_acc()
        self.tick()

        self.data_path.latch_br(sel_br=3, sig_op=Opcode.SUB, sel_op=2**31)
        self.tick()

        if self.data_path.br_negative():
            raise MemoryCorruptedError()

        self.data_path.latch_addr(sel_addr=-1)

        self.data_path.latch_acc()
        self.tick()

        self.data_path.latch_br(sel_br=3, sig_op=Opcode.ADD, sel_op=2)
        self.tick()

        self.data_path.latch_addr(sel_addr=-1)
        self.tick()
        self.data_path.latch_acc(sel_acc=operation)
        self.counter = 0

        while not self.data_path.acc_zero():
            self.data_path.latch_addr(sel_addr=-2)
            self.tick()
            self.data_path.sig_wr()
            self.data_path.latch_acc(sel_acc=operation)
            self.tick()
            self.counter += 1
            assert self.counter <= 64, "Buffer overflow error"

        self.data_path.latch_addr(sel_addr=-1)
        self.tick()

        self.data_path.latch_acc(sel_acc=self.counter)
        self.tick()

        self.data_path.sig_wr()

        self.signal_latch_program_address(operation)
        self.tick()

    def decode_and_execute_io_instruction(self, operation: MemoryCell):
        if operation.code in [Opcode.OUT, Opcode.OUT_PURE]:
            self.execute_out_instruction(operation)
            return

        if operation.code == Opcode.IN:
            self.execute_in_instruction(operation)

    def decode_and_execute_instruction(self):
        """
        Декодирование и исполнение всех инструкций
        Если вместо инструкции была прочитана память данных будет выброшено исключение, то же самое
        произойдет если инструкция была не определена
        """
        operation = self.memory.data[self.program_address]

        if not isinstance(operation, MemoryCell):
            raise MemoryCorruptedError()

        if operation.code in [Opcode.JP, Opcode.JPZ, Opcode.HLT]:
            self.decode_and_execute_control_flow_instruction(operation)
            self.tick()
            return

        if operation.code in alu_op_2_operation.keys():
            self.data_path.latch_op()
            self.data_path.latch_addr(sel_addr=operation.args[0])
            self.tick()

            self.data_path.latch_acc()
            self.tick()

            self.data_path.latch_acc(sel_acc=operation)
            self.signal_latch_program_address(operation)
            self.tick()
            return

        if operation.code == Opcode.LD:
            self.data_path.latch_addr(sel_addr=operation.args[0])
            self.tick()

            self.data_path.latch_acc()
            self.signal_latch_program_address(operation)
            self.tick()
            return

        if operation.code == Opcode.ST:
            self.data_path.latch_addr(sel_addr=operation.args[0])
            self.tick()

            self.data_path.sig_wr()
            self.signal_latch_program_address(operation)
            self.tick()
            return

        if operation.code in [Opcode.OUT, Opcode.OUT_PURE, Opcode.IN]:
            self.decode_and_execute_io_instruction(operation)
            return

        raise MemoryCorruptedError()

    def log(self, program_address: int = 0, verbose_until: int | None = None) -> None:
        data_path_memory_out = self.memory.data[self.data_path.addr_register]
        if isinstance(data_path_memory_out, MemoryCell):
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
        if verbose_until is None:
            logging.debug(state_repr)
            return

        if verbose_until >= self.instr_counter:
            logging.debug(state_repr)
        elif verbose_until + 1 == self.instr_counter:
            logging.debug("VERBOSE LOGGING STOPPED...")


class LogSettings:
    currently_logged_instruction: int = 0
    """
        Объект нacтpoйки логирования
        verbose_instr - сколько инструкций нужно подробно логировать, после достижения счетчиком этого значения
        подробное логирование прекратиться
        log_output_path - если значение установлено логирование будет идти в файл, a не в стандартный поток вывода.
        level - уровень логирования
    """

    def __init__(
        self, verbose_instr: int | None = None, log_output_path: str | None = None, level: str = "DEBUG"
    ) -> None:
        level2int = {"DEBUG": 10, "INFO": 20, "WARN": 30, "ERROR": 40, "CRITICAL": 50}
        self.verbose_instr = verbose_instr
        self.log_output_path = log_output_path
        if level not in level2int.keys():
            level = 10
        else:
            level = level2int[level]
        if log_output_path is not None:
            logging.basicConfig(level=level, filename=log_output_path)
        else:
            logging.basicConfig(level=level)


def simulate(
    code: list[MemoryCell],
    input_buffer: str,
    memory_size: int,
    instruction_limit: int,
    log_settings: LogSettings,
) -> (str, int, int):
    """
    Метод симуляции работы процессора
    На выход передает
    * output_buffer
    * instruction counter
    * tick_counter

    Инициализирует Memory, ControlUnit, DataPath и выполняет последовательное исполнение инструкций
    """
    MemoryUnit.init(code, memory_size)
    if len(MemoryUnit.data) > memory_size:
        logging.critical("Memory limit exceeded")
        return "", 0, 0

    io_unit = IOUnit()
    io_unit.register_port(0, "")
    io_unit.register_port(1, input_buffer)
    data_path = DataPath(io_unit)
    control_unit = ControlUnit(data_path)
    try:
        while control_unit.instr_counter < instruction_limit:
            prev_program_address = control_unit.program_address
            control_unit.decode_and_execute_instruction()

            control_unit.log(prev_program_address, log_settings.verbose_instr)

            control_unit.instr_counter += 1
    except MemoryCorruptedError:
        logging.critical("Memory corrupted")
        pass
    except StopMachineError:
        control_unit.instr_counter += 1
        pass
    except EOFError:
        logging.warning("Input buffer is empty")
        pass

    if control_unit.instr_counter >= instruction_limit:
        logging.warning("Limit exceeded!")

    output_buffer = control_unit.data_path.io_unit.read_all_from_port(0)

    logging.debug(f"Total instructions: {control_unit.instr_counter}")
    logging.debug(f"Total ticks: {control_unit.tick_counter}")
    logging.debug(f"Output buffer: {output_buffer}")
    logging.debug("Execution stopped")

    return output_buffer, control_unit.instr_counter, control_unit.tick_counter


def main(code_file: str, input_file: str, log_settings: LogSettings = LogSettings()):
    code: list[MemoryCell] = read_code(code_file)
    input_tokens = read_file(input_file)
    output, instr_counter, tick_counter = simulate(
        code, input_tokens, memory_size=1000, instruction_limit=100000, log_settings=log_settings
    )

    print(output)


@click.command()
@click.option("--log-output-file", help="File to place machine working logs; Default: Not set")
@click.option(
    "--verbose-instr", help="Amount of instructions to be verbose logged; Default: Not set", default=None, type=int
)
@click.option("--logging-level", help="DEBUG, INFO, WARN, ERROR, CRITICAL; Default: DEBUG", default="DEBUG", type=str)
@click.argument("code", type=click.Path(exists=True))
@click.argument("input_buffer", type=click.Path(exists=True))
def start(log_output_file: str | None, verbose_instr: int | None, code: str, input_buffer: str, logging_level: str):
    """
    Simulator run control interface

    CODE - Machine code file path

    INPUT_BUFFER - Path to file which will emulate input buffer
    """
    log_settings = LogSettings(verbose_instr, log_output_file, logging_level)
    main(code, input_buffer, log_settings)


if __name__ == "__main__":
    start()

from __future__ import annotations

import logging
import sys

import click
from io_helper import read_file
from isa import MemoryCell, Opcode, read_code


class Memory:
    """
         Объект памяти в котором хранятся ячейки с инструкциями и данными
         Инициализируется единожды при старте процессора
         Ссылки на него хранятся в ControlUnit и DataPath
    """
    data: list[MemoryCell | int]

    @staticmethod
    def init(code: list[MemoryCell], limit: int):
        Memory.data = []
        for operation in code:
            if operation.code is Opcode.MEM:
                assert -(2**32) <= operation.arg <= (2**32 - 1), "memory cell is out of bound: {}".format(operation.arg)
                Memory.data.append(operation.arg)
            else:
                Memory.data.append(operation)
        Memory.data.extend([0] * (limit - len(code)))


class MemoryCorruptedError(Exception):
    pass


class StopMachineError(Exception):
    pass


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
    def __init__(self, input_buffer: list):
        self.memory = Memory
        self.addr_register = 0
        self.acc = 0
        self.op_register = 0
        self.input_buffer = input_buffer
        self.output_buffer = []
        self.buffer_register = 0

    def latch_addr(self, sel_addr: int | None = None):
        """     +<----------+
                |           |
            +-----+     +------+
sel_addr -->| MUX | --> | addr |
            +-----+     +------+
            Сигнал защёлкивания регистра адреса, может защёлкнуть либо
            * Новый адрес из сигнала sel_addr
            * Свое старое значение (без изменений)
        """
        if sel_addr is not None:
            self.addr_register = sel_addr

    def latch_acc(self, sel_acc: MemoryCell | int = None):
        """
            Защёлка аккумулятора работает, может защёлкнуть либо
            * Значение текущей ячейки памяти
            * Константу
            * Результат операции
            * Значение из устройства ввода
        """
        if sel_acc is None:
            self.acc = self.memory.data[self.addr_register]
            return

        if isinstance(sel_acc, int):
            assert -(2**32) <= sel_acc <= (2**32 - 1), "value is out of bound: {}".format(sel_acc)
            self.acc = sel_acc
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
        """
            Защёлка регистра операнда
        """
        self.op_register = self.acc

    def latch_br(self):
        """
            Защёлка буферного регистра
        """
        self.buffer_register = self.acc

    def signal_wr(self):
        """
            Сигнал записи в память значения из аккумулятора по текущему адресу
        """
        self.memory.data[self.addr_register] = self.acc

    def signal_output(self, pure: bool = False):
        """
            Сигнал записи в устройство вывода значения из аккумулятора
        """
        if pure:
            self.output_buffer.append(str(self.acc))
            return
        symbol = chr(self.acc)
        self.output_buffer.append(symbol)

    def zero(self):
        """
            Сигнал означающий что в аккумуляторе 0
        """
        return self.acc == 0

    def br(self):
        """
            Сигнал с текущим значением буферного регистра
        """
        return self.buffer_register

    def acc_val(self):
        """
            Сигнал с текущим значением аккумулятора регистра
        """
        return self.acc


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

    def signal_latch_program_address(self, operation: MemoryCell = None):
        """
            Защёлка program_address регистра, может защёлкнуть
            * Текущий адрес + 1
            * Адрес из инструкцию управления
        """
        if operation is not None and operation.code in [Opcode.JP, Opcode.JPZ]:
            self.program_address = operation.arg
        else:
            self.program_address += 1

    def signal_latch_buffer_register(self, val: int):
        """
            Защёлка буфферного регистра
        """
        self.buffer_register = val

    def decode_and_execute_control_flow_instruction(self, operation: MemoryCell):
        """
            Декодирование и исполнение инструкций управления
            HLT - остановка тактового генератора
            JP, JPZ условный и безусловный переходы
        """
        if operation.code == Opcode.HLT:
            raise StopMachineError()

        if operation.code == Opcode.JP or (operation.code == Opcode.JPZ and self.data_path.zero()):
            self.signal_latch_program_address(operation)
            return

        self.signal_latch_program_address()

    @staticmethod
    def translate_pointer(operation: int) -> int | None:
        """
            Выполняет вычисление указателя
            Указатели можно отличить от остальных ячеек в памяти по старшему биту, так как ячейки 32 битные
            То в случае если старший 32 бит = 1, то это указатель и указывает он на ячейку по адресу который
            сохранен в оставшихся 31 битах
        """
        pointer = operation - (2**31)
        if pointer < 0:
            return None

        return pointer

    def execute_out_instruction(self, operation: MemoryCell):
        """
            Выполняет инструкции для работы с устройством вывода
            OUT_PURE команда отправляет текущее значение аккумулятора в устройство вывода без конвертации в ASCII символ
            OUT команда
             * если текущее значение аккумулятора - указатель, декодирует его и запускает цикл на основе counter для
             печати всей строки посимвольно в поток вывода
             * если значение аккумулятора - не указатель, выполняется его отправка в поток вывода с предварительной
             конвертацией в символ ASCII
        """
        if operation.code == Opcode.OUT_PURE:
            self.data_path.signal_output(pure=True)
            self.signal_latch_program_address(operation)
            self.tick()
            return

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
            self.execute_out_instruction(operation)

        if 0 < self.counter <= self.data_path.br():
            self.data_path.latch_addr(sel_addr=(self.buffer_register + self.counter))
            self.tick()

            self.data_path.latch_acc()
            self.tick()

            self.data_path.signal_output()
            self.counter += 1
            self.execute_out_instruction(operation)

        if self.counter != 0:
            self.signal_latch_program_address(operation)
            self.counter = 0
            self.tick()

        return

    def execute_in_instruction(self, operation: MemoryCell):
        """
            Выполняет инструкции для работы с устройством ввода
            IN команда
             * если аргумента нет загружает один символ потока ввода в аккумулятор
             * если аргумент передан и это не указатель выкидывается эксепшен
             * если аргумент указатель начинается посимвольное чтение из потока ввода в указанную область памяти
        """
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
            self.execute_in_instruction(operation)
            self.tick()

        if self.counter >= 1:
            addr = self.buffer_register + self.data_path.br() + self.counter
            if self.data_path.acc_val() != 0:
                assert addr - self.buffer_register <= 64, "Buffer overflow error"
                self.data_path.latch_addr(sel_addr=addr)
                self.tick()
                self.data_path.signal_wr()
                self.data_path.latch_acc(sel_acc=operation)
                self.counter += 1
                self.execute_in_instruction(operation)
            else:
                self.data_path.latch_acc(sel_acc=addr - self.buffer_register - 1)
                self.data_path.latch_addr(sel_addr=self.buffer_register)
                self.tick()
                self.data_path.signal_wr()

        if self.counter != 0:
            self.signal_latch_program_address(operation)
            self.counter = 0
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
        Объект настройки логирования
        verbose_instr - сколько инструкций нужно подробно логировать, после достижения счетчиком этого значения
        подробное логирование прекратиться
        log_output_path - если значение установлено логирование будет идти в файл, а не в стандартный поток вывода.
        level - уровень логирования
    """

    def __init__(self, verbose_instr: int | None = None, log_output_path: str | None = None, level: str = "DEBUG") -> None:
        level2int = {
            "DEBUG": 10,
            "INFO": 20,
            "WARN": 30,
            "ERROR": 40,
            "CRITICAL": 50
        }
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
    input_tokens: list[str],
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

            control_unit.log(prev_program_address, log_settings.verbose_instr)

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


def main(code_file: str, input_file: str, log_settings: LogSettings = LogSettings()):
    code: list[MemoryCell] = read_code(code_file)
    input_tokens = list(read_file(input_file))
    output, instr_counter, tick_counter = simulate(
        code, input_tokens, memory_size=200, instruction_limit=100000, log_settings=log_settings
    )

    print(output)


@click.command()
@click.option("--log-output-file", help="File to place machine working logs; Default: Not set")
@click.option("--verbose-instr", help="Amount of instructions to be verbose logged; Default: Not set", default=None, type=int)
@click.option("--logging-level", help="DEBUG, INFO, WARN, ERROR, CRITICAL; Default: DEBUG", default="DEBUG", type=str)
@click.argument("code", type=click.Path(exists=True))
@click.argument("input_buffer", type=click.Path(exists=True))
def start(log_output_file: str | None, verbose_instr: int | None, code_file: str, input_buffer: str, logging_level: str):
    """
        Simulator run control interface

        CODE - Machine code file path

        INPUT_BUFFER - Path to file which will emulate input buffer
    """
    log_settings = LogSettings(verbose_instr, log_output_file, logging_level)
    main(code_file, input_buffer, log_settings)


if __name__ == "__main__":
    start()

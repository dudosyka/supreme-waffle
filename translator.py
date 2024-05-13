from __future__ import annotations

import re
from parser import parse_expression, program_to_expressions_list

import click
from instruction import Argument, Instruction, Term
from isa import MemoryCell, Opcode, read_file, write_code

# Объект который маппит инструкцию-оператор с её опкодом
# Все инструкции-операторы обрабатываются идентично, объект нужен
# для удобного поиска подобных инструкций
operator2opcode = {
    Term.AND.value: Opcode.AND,
    Term.OR.value: Opcode.OR,
    Term.EQ.value: Opcode.EQ,
    Term.NEQ.value: Opcode.NEQ,
    Term.GR.value: Opcode.GR,
    Term.LW.value: Opcode.LW,
    Term.SUM.value: Opcode.ADD,
    Term.SUB.value: Opcode.SUB,
    Term.DIV.value: Opcode.DIV,
    Term.MOD.value: Opcode.MOD,
    Term.MUL.value: Opcode.MUL,
}


# Класс для более удобного способа хранения процедур на этапе трансляции
class Procedure:
    def __init__(self):
        self.operations: list[MemoryCell] = list()
        self.args: dict[str, int] = dict()
        self.vars_overwrite: dict[str, int] = dict()

    def copy_operations(self):
        return list(
            map(lambda op: MemoryCell(op.code, list(map(int, op.args)), op.calc_flag, op.addr), self.operations)
        )


class Translator:
    def __init__(self):
        self.memory: dict[int, any] = dict()
        self.str_memory: dict[int, int] = dict()
        self.str_memory_pointer: int = 0

        # Лист полученных инструкций языка верхнего уровня
        self.source: list[Instruction] = list()
        # Указатель на текущую свободную ячейку для использования под числовые переменные или константы
        self.memory_pointer: int = 1

        # Хранит доступные пользователю процедуры для последующего инлайна в случае вызова
        self.procedures: dict[str, Procedure] = dict()

        # Маппинг переменных с адресами в памяти
        self.variables: dict[str, int] = dict()

        # Маппинг констант с адресами в памяти
        self.consts: dict[any, int] = dict()

        self.operations: list[MemoryCell] = list()
        self.code: list[MemoryCell] = list()

        self.handlers = {
            Term.SET.value: self.create_or_update_var,
            Term.IF.value: self.translate_conditional_instruction,
            Term.LOOP.value: self.translate_loop,
            Term.PRINT.value: self.translate_print,
            Term.PRINT_INT.value: self.translate_print,
            Term.INPUT.value: self.translate_input,
            Term.RETURN.value: lambda instr: self.operations.append(
                MemoryCell(Opcode.JP, [len(self.operations)], calc_flag=2)
            ),
        }

    op_type = operator2opcode.keys()

    def add_memory_cell(self, value: int = 0):
        """
        Выделяет в памяти переменных и констант ячейку и заполняет ее значением
        """
        self.memory[self.memory_pointer] = value
        self.memory_pointer += 1

    def translate_procedure(self, instruction: Instruction):
        """
        Выполняет транслцию процедуры в набор инструкций, выделяет в памяти ячейки под аргументы

        Поддержка вызовов на уровне машинного кода отсутствует поэтому транслированное тело процедуры
        сохраняется и при нахождении инструкции вызова внутри исходного кода тело процедуры инлайниться
        """
        proc = Procedure()
        overwrite: dict[str, int] = {}
        created: list[str] = []
        # Firstly map local variables if they exist to memory
        for var in instruction.arguments[1].value:
            proc.args[var] = self.memory_pointer
            if var in self.variables.keys():
                overwrite[var] = self.variables[var]
            else:
                created.append(var)
            self.variables[var] = self.memory_pointer
            self.add_memory_cell()

        self.translate(instruction.arguments[2].value)
        self.procedures[instruction.arguments[0].value] = proc

        for var in proc.vars_overwrite.keys():
            self.variables[var] = proc.vars_overwrite[var]
        for var in created:
            self.variables.pop(var)

        for operation in self.operations:
            if operation.calc_flag is not None and operation.calc_flag > 1:
                operation.args[0] = int(len(self.operations) - operation.args[0])
                operation.calc_flag = 1

            self.procedures[instruction.arguments[0].value].operations.append(operation)

    def inline_procedure(self, call: Instruction):
        proc = self.procedures[call.name]
        for i in range(len(call.arguments)):
            arg = call.arguments[i]
            self.translate_arg(arg)
            self.operations.append(MemoryCell(Opcode.ST, list(proc.args.values())[i]))
        self.operations.extend(proc.copy_operations())

    def translate_operator(self, instruction: Instruction) -> list[MemoryCell]:
        """
        Выполняет трансляцию инструкций являющихся математическими операторами
        В случае необходимости сам вызывает методы трансляции для операндов не являющихся константами
        """
        simplified_operands: list[int] = []
        operations = []

        for i in range(len(instruction.arguments)):
            argument = instruction.arguments[i]
            if argument.type == "instr":
                if argument.value.name in self.op_type:
                    operations.extend(self.translate_operator(argument.value))
                else:
                    self.translate([argument.value])
                operations.append(MemoryCell(Opcode.ST, self.memory_pointer))
                simplified_operands.append(self.memory_pointer)
                self.add_memory_cell()
            if argument.type == "val":
                literal_pointer = self.get_const_pointer(argument.value)
                simplified_operands.append(literal_pointer)
            if argument.type == "var":
                simplified_operands.append(self.variables[argument.value])

        operations.append(MemoryCell(Opcode.LD, simplified_operands[0]))

        for op in simplified_operands[1:]:
            operations.append(MemoryCell(operator2opcode[instruction.name], op))

        return operations

    def translate_conditional_instruction(self, instruction: Instruction, ignore_next_jp: int = 0):
        """
        Выполняет трансляцию условий
        Сам вызывает метод трансляции условия перехода и вставляет необходимые джампы.
        Джампы помечаются через calc_flag - это означает что адреса в них относительные и они будут вычислены на финальном этапе
        """
        self.operations.extend(self.translate_operator(instruction.arguments[0].value))
        start_with = len(self.operations)

        self.operations.append(MemoryCell(Opcode.JPZ, 0, 1))
        self.translate(instruction.arguments[1].value)
        if len(instruction.arguments) == 3:
            self.translate_arg(instruction.arguments[2])
        body_end = len(self.operations)
        # Подставляем относительный адрес
        self.operations[start_with].args[0] = body_end - start_with + ignore_next_jp

    def get_const_pointer(self, value: any) -> int:
        """
        Метод для сохранения и получения указателей на константы в памяти
        Если константа уже была сохранена возвращается указатель на нее, если
        такой константы ещё не было она записывается в память и возвращается указатель на нее
        """
        if self.consts.__contains__(value):
            return self.consts[value]

        mem_val = value
        if isinstance(value, str):
            pointer = self.str_memory_pointer + (2**31)
            str_value = value[1 : len(value) - 1]
            str_value = str_value.replace("\\s", " ")
            str_value = str_value.replace("\\n", "\n")
            value_len = len(str_value)
            self.str_memory[self.str_memory_pointer] = value_len
            for char in str_value:
                self.str_memory_pointer += 1
                self.str_memory[self.str_memory_pointer] = ord(char)

            self.str_memory_pointer += 1
            self.consts[value] = self.memory_pointer
            self.add_memory_cell(pointer)
            return self.get_const_pointer(value)

        self.consts[value] = self.memory_pointer
        self.add_memory_cell(mem_val)

        return self.get_const_pointer(value)

    def translate_arg(self, argument: Argument) -> None:
        """
        Выполняет трансляюцию аргументов для других операций
        Инструкцию для которых выполняется трансляцию аргументов, основывается на том,
        что значение требуемого аргумента загрузится в аккумулятор
        """
        if argument.type == "instr":
            self.translate([argument.value])
        if argument.type == "var":
            self.operations.append(MemoryCell(Opcode.LD, self.variables[argument.value]))
        if argument.type == "val":
            literal_pointer = self.get_const_pointer(argument.value)
            self.operations.append(MemoryCell(Opcode.LD, literal_pointer))

    def create_or_update_var(self, instr: Instruction) -> None:
        """
        Выполняет транслцию инструкций управления переменными
        Если требуется создать переменную под нее выделяется память
        и прописывается инициализирующее значение если оно было задано
        Получение инициализационного значение происходит при помощи метода translate arg
        """
        name: str = instr.arguments[0].value
        value: Argument = instr.arguments[1]

        self.translate_arg(value)

        if self.variables.__contains__(name):
            self.operations.append(MemoryCell(Opcode.ST, self.variables[name]))
        else:
            self.variables[name] = self.memory_pointer
            self.operations.append(MemoryCell(Opcode.ST, self.memory_pointer))
            self.add_memory_cell()

    def create_str_var(self, name: str) -> int:
        """
        Управляет создание иключительно строковых переменных так как метод их хранения
        отличается от остальных.
        При этом в памяти переменных инициализируется ячейка в которую помещается указатель на созданную строку
        Инструкцию работающие со строками получают в качестве аргумента ячейку с указателем на строку

        Максимальная длина строки в данной реализации ограничена до 64 символов
        """
        pointer = self.str_memory_pointer + (2**31)
        self.str_memory[self.str_memory_pointer] = 0
        for i in range(64):
            self.str_memory_pointer += 1
            self.str_memory[self.str_memory_pointer] = 0

        self.str_memory_pointer += 1
        self.variables[name] = self.memory_pointer
        self.add_memory_cell(pointer)

        return self.variables[name]

    def translate_memory(self):
        diff = 1 + self.memory_pointer + len(self.operations)
        for i in range(1, self.memory_pointer):
            if self.memory[i] >= 2**31:
                self.memory[i] += diff
            self.code.append(MemoryCell(Opcode.MEM, self.memory[i]))

    def update_relative_jumps(self):
        cur = self.memory_pointer
        for operation in self.code[self.memory_pointer :]:
            # Для всех команд с относительной адресацией рассчитываем абсолютные адреса
            if operation.calc_flag is not None:
                operation.args[0] = cur + (operation.args[0] * operation.calc_flag)
            cur += 1

    def translate_print(self, instr: Instruction):
        for arg in instr.arguments:
            self.translate_arg(arg)
            if instr.name == "print":
                self.operations.append(MemoryCell(Opcode.OUT, 0))
            else:
                self.operations.append(MemoryCell(Opcode.OUT_PURE, 0))

    def translate_input(self, instr: Instruction):
        if len(instr.arguments) > 0:
            arg = instr.arguments[0]
            assert arg.type == "var", "input argument must be var"
            pointer = self.create_str_var(arg.value)
            self.operations.append(MemoryCell(Opcode.IN, [1, pointer]))
        else:
            self.operations.append(MemoryCell(Opcode.IN, [1, None]))

    def translate_loop(self, instr: Instruction):
        start_with = len(self.operations)
        self.translate_conditional_instruction(instr, ignore_next_jp=1)
        self.operations.append(MemoryCell(Opcode.JP, len(self.operations) - start_with, -1))

    def translate(  # noqa: C901
        self, input_source: list[Instruction], proc_defs: list[Instruction] | None = None
    ) -> (int, list[MemoryCell]):
        """
        Основной цикл трансляиии инструкций
        """

        if proc_defs is not None:
            for instr in proc_defs:
                self.operations = []
                self.translate_procedure(instr)
            self.operations = []

        for instr in input_source:
            if instr.name in self.op_type:
                self.operations.extend(self.translate_operator(instr))

            if instr.name in self.procedures.keys():
                self.inline_procedure(instr)

            if instr.name in self.handlers.keys():
                self.handlers[instr.name](instr)

        if proc_defs is not None:
            self.code = [
                MemoryCell(Opcode.JP, self.memory_pointer),
            ]

            self.translate_memory()
            self.code.extend(self.operations)
            self.update_relative_jumps()
            self.code.append(MemoryCell(Opcode.HLT))

            for i in self.str_memory:
                cell = self.str_memory[i]
                self.code.append(MemoryCell(Opcode.MEM, cell))

            return len(self.operations) + 2, self.code

        return 0, ""


def main(source: str, output: str):
    input_code = read_file(source)
    lines = len(input_code.split("\n")) - 1
    # Удаляем комментарии и лишние пробелы
    input_code = re.sub(
        "\\s+\\)",
        ")",
        re.sub("\\(\\s+", "(", re.sub(r"--.+$", "", input_code, flags=re.MULTILINE), flags=re.MULTILINE),
        flags=re.MULTILINE,
    )

    # Получаем список экспрешенов верхнего уровня
    expressions = program_to_expressions_list(input_code)

    parsed_instructions = list()
    proc_definitions = list()
    for expression in expressions:
        instruction = parse_expression(input_code[expression.start_index + 1 : expression.end_index])
        if instruction.name == "deproc":
            proc_definitions.append(instruction)
            continue
        parsed_instructions.append(instruction)

    translator = Translator()

    amount, operations = translator.translate(parsed_instructions, proc_definitions)
    print("source LoC:", lines + 1, "code instr:", amount)

    write_code(output, operations)


@click.command()
@click.argument("source-code", type=click.Path(exists=True))
@click.argument("machine-code", type=click.Path(exists=True))
def start(source_code: str, machine_code: str):
    """
    Translator run control interface

    SOURCE_CODE - Path to source code file

    MACHINE_CODE - Path to output file, where machine code will be placed
    """
    main(source_code, machine_code)


if __name__ == "__main__":
    start()

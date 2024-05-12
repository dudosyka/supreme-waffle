from __future__ import annotations

import re
import sys
from parser import parse_expression, program_to_expressions_list

from instruction import Argument, Instruction, Term
from io_helper import read_file
from isa import Opcode, Operation, write_code

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
        self.operations: list[Operation] = list()
        self.args: dict[str, int] = dict()


class Translator:
    def __init__(self):
        self.memory: dict[int, any] = dict()
        self.dynamic_memory: dict[int, int] = dict()
        self.dynamic_memory_pointer: int = 0

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

        self.operations: list[Operation] = list()
        self.code: list[Operation] = list()

    op_type = operator2opcode.keys()

    def add_memory_cell(self, value: int = 0):
        self.memory[self.memory_pointer] = value
        self.memory_pointer += 1

    def translate_function(self, instruction: Instruction) -> str:
        func = Procedure()
        # Firstly map local variables if they exist to memory
        for var in instruction.arguments[1].value:
            func.args[var] = self.memory_pointer
            self.variables[var] = self.memory_pointer
            self.add_memory_cell()
        self.translate(instruction.arguments[2].value)
        self.procedures[instruction.arguments[0].value] = func

        return instruction.arguments[0].value

    def translate_operator(self, instruction: Instruction) -> list[Operation]:
        simplified_operands: list[int] = []
        operations = []

        for i in range(len(instruction.arguments)):
            argument = instruction.arguments[i]
            if argument.type == "instr":
                operations.extend(self.translate_operator(argument.value))
                operations.append(Operation(Opcode.ST, self.memory_pointer))
                simplified_operands.append(self.memory_pointer)
                self.add_memory_cell()
            if argument.type == "val":
                literal_pointer = self.get_const_pointer(argument.value)
                simplified_operands.append(literal_pointer)
            if argument.type == "var":
                simplified_operands.append(self.variables[argument.value])

        operations.append(Operation(Opcode.LD, simplified_operands[0]))

        for op in simplified_operands[1:]:
            operations.append(Operation(operator2opcode[instruction.name], op))

        return operations

    # Метод предназначен для обработки управляющих инструкций с условиями (if \ loop)
    def translate_conditional_instruction(self, instruction: Instruction, ignore_next_jp: int = 0):
        self.operations.extend(self.translate_operator(instruction.arguments[0].value))
        start_with = len(self.operations)
        # После обработки условия вставляется джамп без адреса,
        # относительный адрес будет установлен после обработки основного блока инструкций
        # флаг calc_flag означает что абсолютный адрес будет вычислен на последнем этапе
        self.operations.append(Operation(Opcode.JPZ, 0, 1))
        self.translate(instruction.arguments[1].value)
        body_end = len(self.operations)
        # Подставляем относительный адрес
        self.operations[start_with].arg = body_end - start_with + ignore_next_jp

    def get_const_pointer(self, value: any) -> int:
        if self.consts.__contains__(value):
            return self.consts[value]

        mem_val = value
        if isinstance(value, str):
            pointer = self.dynamic_memory_pointer + (2**31)
            value = value[1:len(value) - 1]
            value = value.replace("\\s", " ")
            value = value.replace("\\n", "\n")
            value_len = len(value)
            self.dynamic_memory[self.dynamic_memory_pointer] = value_len
            for char in value:
                self.dynamic_memory_pointer += 1
                self.dynamic_memory[self.dynamic_memory_pointer] = ord(char)

            self.dynamic_memory_pointer += 1
            self.consts[value] = self.memory_pointer
            self.add_memory_cell(pointer)
            return self.get_const_pointer(value)

        self.consts[value] = self.memory_pointer
        self.add_memory_cell(mem_val)

        return self.get_const_pointer(value)

    def translate_arg(self, argument: Argument) -> None:
        if argument.type == "instr":
            if argument.value.name in self.op_type:
                self.operations.extend(self.translate_operator(argument.value))
            if argument.value.name == "input":
                self.translate([argument.value])
        if argument.type == "var":
            self.operations.append(Operation(Opcode.LD, self.variables[argument.value]))
        if argument.type == "val":
            literal_pointer = self.get_const_pointer(argument.value)
            self.operations.append(Operation(Opcode.LD, literal_pointer))

    def create_or_update_var(self, name: str, value: Argument) -> None:
        self.translate_arg(value)

        if self.variables.__contains__(name):
            self.operations.append(Operation(Opcode.ST, self.variables[name]))
        else:
            self.variables[name] = self.memory_pointer
            self.operations.append(Operation(Opcode.ST, self.memory_pointer))
            self.add_memory_cell()

    def create_str_var(self, name: str) -> int:
        pointer = self.dynamic_memory_pointer + (2**31)
        self.dynamic_memory[self.dynamic_memory_pointer] = 0
        for i in range(64):
            self.dynamic_memory_pointer += 1
            self.dynamic_memory[self.dynamic_memory_pointer] = 0

        self.dynamic_memory_pointer += 1
        self.variables[name] = self.memory_pointer
        self.add_memory_cell(pointer)

        return self.variables[name]

    # Так как метод translate использует для трансляции инструкций на любом уровне
    # требуется флаг top_lvl который выставляется лишь в самом первом вызове
    # Только такие вызовы выполняю поиск инструкций для трансляции процедур и возвращают итоговый результат трансляции
    def translate(self, input_source: list[Instruction], top_lvl: bool = False) -> (int, list[Operation]):  # noqa: C901
        if top_lvl:
            # Firstly find all functions it can be only top-lvl instr
            for instr in input_source:
                if instr.name == Term.DEPROC.value:
                    self.operations = []
                    func_name = self.translate_function(instr)
                    self.procedures[func_name].operations = self.operations

            self.operations = []

        common = list(filter(lambda instr: instr.name != Term.DEPROC.value, input_source))

        for instr in common:
            if instr.name == "set":
                self.create_or_update_var(instr.arguments[0].value, instr.arguments[1])
            if instr.name in Translator.op_type:
                self.operations.extend(self.translate_operator(instr))
            if instr.name == "if":
                self.translate_conditional_instruction(instr)
            if instr.name == "loop":
                start_with = len(self.operations)
                self.translate_conditional_instruction(instr, ignore_next_jp=1)
                self.operations.append(Operation(Opcode.JP, len(self.operations) - start_with, -1))
            if instr.name in self.procedures.keys():
                func = self.procedures[instr.name]
                for i in range(len(instr.arguments)):
                    arg = instr.arguments[i]
                    self.translate_arg(arg)
                    self.operations.append(Operation(Opcode.ST, list(func.args.values())[i]))
                self.operations.extend(func.operations)
            if instr.name == "print":
                for arg in instr.arguments:
                    self.translate_arg(arg)
                    self.operations.append(Operation(Opcode.OUT))
            if instr.name == "print_int":
                for arg in instr.arguments:
                    self.translate_arg(arg)
                    self.operations.append(Operation(Opcode.OUT_PURE))
            if instr.name == "input":
                if len(instr.arguments) > 0:
                    arg = instr.arguments[0]
                    assert arg.type == "var", "input argument must be var"
                    pointer = self.create_str_var(arg.value)
                    self.operations.append(Operation(Opcode.IN, pointer))
                else:
                    self.operations.append(Operation(Opcode.IN))

        if top_lvl:
            self.code = [
                Operation(Opcode.JP, self.memory_pointer),
            ]
            diff = 1 + self.memory_pointer + len(self.operations)
            for i in range(1, self.memory_pointer):
                if self.memory[i] >= 2**31:
                    self.memory[i] += diff
                self.code.append(Operation(Opcode.MEM, self.memory[i]))
            self.code.extend(self.operations)
            cur = self.memory_pointer
            for operation in self.code[self.memory_pointer:]:
                # Для всех команд с относительной адресацией рассчитываем абсолютные адреса
                if operation.calc_flag is not None:
                    operation.arg = cur + (operation.arg * operation.calc_flag)
                cur += 1

            self.code.append(Operation(Opcode.HLT))

            for i in self.dynamic_memory:
                cell = self.dynamic_memory[i]
                self.code.append(Operation(Opcode.MEM, cell))

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
    for expression in expressions:
        instruction = parse_expression(input_code[expression.start_index + 1 : expression.end_index])
        parsed_instructions.append(instruction)

    translator = Translator()

    amount, operations = translator.translate(parsed_instructions, True)
    print("source LoC:", lines, "code instr:", amount)

    write_code(output, operations)


if __name__ == "__main__":
    assert len(sys.argv) == 3, "Wrong arguments: translator.py <input_file> <target_file>"
    _, source, target = sys.argv
    main(source, target)

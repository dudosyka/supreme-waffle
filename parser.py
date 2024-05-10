from __future__ import annotations

from instruction import Argument, Instruction, available_instructions


class ParsedExpression:
    start_index: int
    end_index: int

    def __init__(self, start_index: int, end_index: int):
        self.start_index = start_index
        self.end_index = end_index


def program_to_expressions_list(code: str) -> list[ParsedExpression]:
    terms = list(code)
    expressions: list[ParsedExpression] = list()
    search_closer: int = 0
    cur_start_index: int = 0
    for pointer in range(len(terms)):
        char = terms[pointer]
        if char == "(":
            if search_closer == 0:
                cur_start_index = pointer
            search_closer += 1
        if char == ")":
            search_closer -= 1
            assert search_closer >= 0, "Bad particles sequence"
            if search_closer == 0:
                expressions.append(ParsedExpression(cur_start_index, pointer))

    return expressions


class Parser:
    validators = dict()  # noqa: RUF012
    available_functions = []  # noqa: RUF012

    def __init__(self, name: str, code: str):
        self.name = name
        self.code = code
        self.instr = Instruction("")

    @staticmethod
    def is_opener(term: str) -> (bool, str):
        split = list(term)
        if split[0] == "(":
            return True, "".join(split[1:])

        return False, "".join(split)

    @staticmethod
    def get_simple_operand_type(term: str) -> (str, str, int):
        split = list(term)
        # If it doesn't start with number and with quotes than it probably variable
        if split[0] != '"' and split[0] not in "1234567890":
            return "var", "".join(split), len(split) + 1
        # If it starts from quote or from number its concrete value

        diff = len(split) + 1
        if split[0] == '"':
            return "val", term, diff

        return "val", int(term), diff

    def unified_parse_loop(self):  # noqa: C901
        i = 0
        while i < len(self.code):
            cut = self.code[i:len(self.code)]
            terms = self.code[i:len(self.code)].split()
            term = terms[0]
            is_opener, operand = self.is_opener(term)
            if is_opener:
                if self.name == "deproc" and len(self.instr.arguments) == 1:
                    args = self.code[i:len(self.code)].split(") (")[0][1:]
                    operand = Argument("var", args.split())
                    i += len(self.code[i:len(self.code)].split(") (")[0]) + 2
                    Parser.available_functions.append(self.instr.arguments[0].value)
                else:
                    instr_list = []
                    for parsed in program_to_expressions_list(self.code[i:len(self.code)]):
                        items = cut[parsed.start_index:parsed.end_index].split()
                        instruction_name = "".join(list(items[0])[1:])
                        parser = Parser(instruction_name, " ".join(items[1:]))
                        instr_list.append(parser.parse())
                        i += (parsed.end_index + 1)

                    if self.name == "deproc":
                        operand = Argument("instr_list", instr_list)
                    elif (self.name == "if" or self.name == "loop") and len(self.instr.arguments) == 0:
                        self.instr.arguments.append(Argument("instr", instr_list[0]))
                        operand = Argument("instr_list", instr_list[1:])
                    else:
                        for arg in instr_list[0:len(instr_list) - 1]:
                            operand = Argument("instr", arg)
                            self.instr.arguments.append(operand)
                        operand = Argument("instr", instr_list[len(instr_list) - 1])
            else:
                optype, value, diff = self.get_simple_operand_type(term)
                operand = Argument(optype, value)
                i += diff

            self.instr.arguments.append(operand)

    def parse(self) -> Instruction:
        assert (
                self.name in available_instructions or
                self.name in Parser.available_functions
        ), f"Syntax error unknown instruction {self.name}"

        self.instr = Instruction(self.name)
        self.unified_parse_loop()

        if self.name in self.validators.keys():
            validator = self.validators[self.name]
            assert not validator(self.instr), f"Syntax error while parsing instruction {self.name}"

        return self.instr


def parse_expression(instruction: str) -> Instruction:
    terms = instruction.split()
    instruction_name = terms[0]
    parser = Parser(instruction_name, " ".join(terms[1:]))
    return parser.parse()


def print_expression(input_code: str, instructions: list) -> None:
    i = 0
    for instruction in instructions:
        i += 1
        print(f"#{i}: {input_code[instruction.start_index:instruction.end_index + 1]}")

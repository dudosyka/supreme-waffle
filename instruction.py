from __future__ import annotations

from enum import Enum

available_instructions = [
    "set", "input", "print", "print_int",
    "deproc", "if", "loop",
    "+", "-", "/", "*",
    "%", "and", "or",
    "=", "!=", ">", "<",
]


class Term(str, Enum):
    SET = "set"
    LOOP = "loop"
    IF = "if"
    DEPROC = "deproc"
    INPUT = "input"
    PRINT = "print"
    PRINT_INT = "print integer"
    AND = "and"
    OR = "or"
    EQ = "="
    NEQ = "!="
    GR = ">"
    LW = "<"
    SUM = "+"
    SUB = "-"
    DIV = "/"
    MOD = "%"
    MUL = "*"


class Argument:
    type: str = None
    value = None

    def __init__(self, optype: str, value: any):
        self.type = optype
        self.value = value

    def print(self):
        if self.type == "instr":
            print(f"instr({self.value.name})")
        elif self.type == "instr_list":
            for value in self.value:
                print(f"instr({value.name})")
        else:
            print(f"{self.type}: {self.value}")

    def to_string(self) -> str:
        if self.type == "instr":
            return f"instr({self.value.to_string()})"

        if self.type == "instr_list":
            result_list = []
            for value in self.value:
                result_list.append(f"instr({value.to_string()})")
            return str(result_list)

        return f"{self.type}: {self.value}"


class Instruction:
    name: str
    arguments: list[Argument]

    def __init__(self, name):
        self.name = name
        self.arguments = list()

    def print(self):
        print("Instruction")
        print(self.name)
        for op in self.arguments:
            op.print()

    def to_string(self):
        return f"{self.name}({list(map(lambda x: x.to_string(), self.arguments))})"

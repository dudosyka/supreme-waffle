from __future__ import annotations

from enum import Enum

from io_helper import read_file, write_file


# Enum всех доступных команд процессора
class Opcode(str, Enum):
    LD = "load"
    ST = "store"
    ADD = "add"
    SUB = "sub"
    MUL = "multiplex"
    DIV = "divide"
    MOD = "modulus of division"
    AND = "and"
    OR = "or"
    EQ = "equal"
    NEQ = "not equal"
    GR = "Grater"
    LW = "Lower"
    JP = "jump"
    JPZ = "jump not zero"
    IN = "input"
    OUT = "output"
    OUT_PURE = "output pure"
    HLT = "halt"
    MEM = "no operation"


# Класс описывающий объект операции на уровне системы команд
class Operation:
    def __init__(self, code: Opcode, arg: int | None = None, calc_flag: int | None = None, addr: int | None = None):
        self.code: Opcode = code
        self.arg: int | None = arg
        self.calc_flag: int | None = calc_flag
        self.addr: int | None = addr

    def to_string(self) -> str:
        res = f"{self.code.name}"
        if self.arg is not None:
            res += f" {self.arg!s}"
        if self.addr is not None:
            res += f" @{self.addr}"

        return res


str2opcode = {
    "LD": Opcode.LD,
    "ST": Opcode.ST,
    "ADD": Opcode.ADD,
    "SUB": Opcode.SUB,
    "MUL": Opcode.MUL,
    "DIV": Opcode.DIV,
    "MOD": Opcode.MOD,
    "AND": Opcode.AND,
    "OR": Opcode.OR,
    "EQ": Opcode.EQ,
    "NEQ": Opcode.NEQ,
    "GR": Opcode.GR,
    "LW": Opcode.LW,
    "JP": Opcode.JP,
    "JPZ": Opcode.JPZ,
    "IN": Opcode.IN,
    "OUT": Opcode.OUT,
    "OUT_PURE": Opcode.OUT_PURE,
    "HLT": Opcode.HLT,
    "MEM": Opcode.MEM,
}


def write_code(filename: str, operations: list[Operation]) -> None:
    code = ""
    for j in range(len(operations)):
        code += f"{operations[j].to_string()}\n"

    write_file(filename, code)


def read_code(filename: str) -> list[Operation]:
    code = read_file(filename)
    operations: list[Operation] = []
    lines: list[str] = code.split("\n")
    for i in range(len(lines)):
        line = lines[i]
        split = line.split()
        if len(split) == 3:
            opcode_str, arg_str, pointer_arg_str = split
            arg = int(arg_str)
        elif len(split) == 2:
            opcode_str, arg_str = split
            arg = int(arg_str)
        elif len(split) == 1:
            opcode_str = split[0]
            arg = None
        else:
            continue
        operations.append(Operation(str2opcode[opcode_str], arg, addr=i))

    return operations

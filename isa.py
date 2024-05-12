from __future__ import annotations

from enum import Enum

from io_helper import read_file, write_file


class Opcode(str, Enum):
    """
    Перечисление всех доступных команд
    Все опкоды кроме MEM являются инструкциями,
    При инициализации памяти MEM парсится в числовую ячейку
    """

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
    MEM = "memory cell"


class MemoryCell:
    """
    Класс описывающий объект памяти
    """

    def __init__(
        self, code: Opcode, args: int | list[int] | None = None, calc_flag: int | None = None, addr: int | None = None
    ):
        self.code: Opcode = code
        self.args: list[int] | None = args
        if isinstance(args, int):
            self.args = [args]
        elif isinstance(args, list):
            self.args = args
        self.calc_flag: int | None = calc_flag
        self.addr: int | None = addr

    def to_string(self) -> str:
        res = f"{self.code.name}"
        if self.args is not None:
            for arg in self.args:
                if arg is not None:
                    res += f" {arg!s}"
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


def write_code(filename: str, operations: list[MemoryCell]) -> None:
    code = ""
    for j in range(len(operations)):
        code += f"{operations[j].to_string()}\n"

    write_file(filename, code)


def read_code(filename: str) -> list[MemoryCell]:
    code = read_file(filename)
    operations: list[MemoryCell] = []
    lines: list[str] = code.split("\n")
    for i in range(len(lines)):
        line = lines[i]
        split = line.split()
        if len(split) >= 2:
            opcode_str = split[0]
            args = list(map(int, split[1:]))
        elif len(split) == 1:
            opcode_str = split[0]
            args = None
        else:
            continue
        operations.append(MemoryCell(str2opcode[opcode_str], args, addr=i))

    return operations

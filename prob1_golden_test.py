import contextlib
import io
import logging
import os
import tempfile

import machine
import pytest
import translator


def get_first_lines(n: int, output: str):
    return "\n".join(output.split("\n")[0:n])


@pytest.mark.golden_test("golden/prob1.yml")
def test_translator_and_machine(golden, caplog):
    """Используется подход golden tests. У него не самая удачная реализация для
    python: https://pypi.org/project/pytest-golden/ , но знать об этом подходе
    крайне полезно.

    Принцип работы следующий: во внешних файлах специфицируются входные и
    выходные данные для теста. При запуске тестов происходит сравнение и если
    выход изменился -- выводится ошибка.

    Если вы меняете логику работы приложения -- то запускаете тесты с ключом:
    `cd python && poetry run pytest . -v --update-goldens`

    Это обновит файлы конфигурации, и вы можете закоммитить изменения в
    репозиторий, если они корректные.

    Формат файла описания теста -- YAML. Поля определяются доступом из теста к
    аргументу `golden` (`golden[key]` -- входные данные, `golden.out("key")` --
    выходные данные).

    Вход:

    - `in_source` -- исходный код
    - `in_stdin` -- данные на ввод процессора для симуляции

    Выход:

    - `out_code` -- машинный код, сгенерированный транслятором
    - `out_stdout` -- стандартный вывод транслятора и симулятора
    - `out_log` -- журнал программы
    """
    # Установим уровень отладочного вывода на DEBUG
    caplog.set_level(logging.DEBUG)

    # Создаём временную папку для тестирования приложения.
    with tempfile.TemporaryDirectory() as tmpdirname:
        # Готовим имена файлов для входных и выходных данных.
        source = os.path.join(tmpdirname, "source.lisp")
        input_stream = os.path.join(tmpdirname, "input.txt")
        target = os.path.join(tmpdirname, "target.o")

        # Записываем входные данные в файлы. Данные берутся из теста.
        with open(source, "w", encoding="utf-8") as file:
            file.write(golden["in_source"])
        with open(input_stream, "w", encoding="utf-8") as file:
            file.write(golden["in_stdin"])

        # Запускаем транслятор и собираем весь стандартный вывод в переменную
        # stdout
        with contextlib.redirect_stdout(io.StringIO()) as stdout:
            translator.main(source, target)
            print("============================================================")
            # За 269 инструкций симуляция успеет посчитать первые 9 чисел.
            # Так как основной поток вычислений идёт в цикле этого достаточно,
            # для того, чтобы понять, что всё работает корректно
            machine.main(target, input_stream, machine.LogSettings(verbose_instr=269))

        # Выходные данные также считываем в переменные.
        with open(target, encoding="utf-8") as file:
            code = file.read()

        # Проверяем, что ожидания соответствуют реальности.
        assert code == golden.out["out_code"]
        assert stdout.getvalue() == golden.out["out_stdout"]
        assert caplog.text == golden.out["out_log"]

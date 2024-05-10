def read_file(file_name: str) -> str:
    with open(file_name) as file:
        return file.read()


def write_file(file_name: str, output: str) -> None:
    with open(file_name, "w") as file:
        file.write(output)

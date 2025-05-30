def read_classification_from_file(directory):
    with open(directory, 'rt', encoding='utf-8') as f:
        text = f.read()

    d = {}

    lines = text.split()
    for i in range(len(lines)):
        if i % 2 == 0:
            d.update({lines[i]:lines[i + 1]})

    """for i in range(len(list(d.keys()))):
        print(f"{list(d.keys())[i]} is {list(d.values())[i]}")"""

    return d


if __name__ == "__main__":
    read_classification_from_file('.\\spam-data-12-s75-h25\\1\\!truth.txt')

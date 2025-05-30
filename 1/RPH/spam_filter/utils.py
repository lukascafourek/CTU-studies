def read_classification_from_file(filename):
    with open(filename, 'rt', encoding='utf-8') as f:
        file = f.read()
    dictionary = {}
    if file:
        for row in file.split('\n'):
            x = row.split()
            if x:
                dictionary.update({x[0]: x[1]})
    else:
        dictionary = {}
    f.close()
    return dictionary

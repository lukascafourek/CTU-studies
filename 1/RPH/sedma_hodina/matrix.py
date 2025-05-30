class MyMatrix:

    def __init__(self, matrix=None):
        self.matrix = matrix

    def save(self, filename):
        with open(filename, 'wt', encoding='utf-8') as f:
            matrix = []
            for row in self.matrix:
                for i in range(len(row)):
                    row[i] = str(row[i])
                matrix.append(' '.join(row))
            f.write('\n'.join(matrix))

    def load(self, filename):
        with open(filename, 'rt', encoding='utf-8') as f:
            matrix = f.read()
        self.matrix = []
        for r in matrix.split('\n'):
            row = []
            for num in r.split(' '):
                row.append(int(num))
            self.matrix.append(row)
        print(self.matrix)


if __name__ == "__main__":
    a = MyMatrix([[1, 2, 3], [2, 3, 4]])
    a.save('matrix.txt')
    b = MyMatrix()
    b.load('matrix.txt')

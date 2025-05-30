import os


class Corpus:

    def __init__(self, path = None):
        self.path = path

    def emails(self):
        dir = os.listdir(os.getcwd() + self.path)
        for line in dir:
            if line.startswith('!'):
                dir.remove(line)
            else:
                text = line.split(".")
                yield text[0], text[1]


if __name__=="__main__":
    directory = "\\spam-data-12-s75-h25\\1"
    corpus = Corpus(directory)
    count = 0
    for fname, body in corpus.emails():
        print(fname)
        print(body)
        print('-------------------------')
        count += 1
    print('Finished: ', count, 'files processed.')

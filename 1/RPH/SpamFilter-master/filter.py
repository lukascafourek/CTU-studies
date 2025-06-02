import os

class MyFilter:

    def __init__(self):
        self.spam_words = set()
        self.ok_words = set()

    def train(self, train_corpus_dir):
        spam_words = self.spam_words
        ok_words = set()
        with open(train_corpus_dir + "\\!truth.txt", 'rt', encoding='utf-8') as f:
            text = f.read().split()
            for i in range(0, len(text), 2):
                email = text[i]
                result = text[i+1]
                if result == "SPAM":
                    spam_words = self.load_words(train_corpus_dir, email, spam_words)
                if result == "OK":
                    ok_words = self.load_words(train_corpus_dir, email, ok_words)
                if result != "SPAM" and result != "OK":
                    print(f"Error: unexpected result! {result}")

        spam_words = self.remove_common(ok_words, spam_words)
        spam_words = self.leave_non_alpha(spam_words)
        # print(f"Final spam words\n{spam_words}\nFinal spam words")
        self.spam_words = spam_words

    def load_words(self, train_corpus_dir, email, word_set):
        with open(train_corpus_dir + "\\" + email, "rt", encoding='utf-8') as f:
            word_list = f.read().split(sep=' ')
            for word in word_list:
                word_set.add(word)
        return word_set

    def remove_common(self, ok_words, spam_words):
        for word in ok_words:
            spam_words.discard(word)
        return spam_words

    def leave_non_alpha(self, spam_words):
        word_set = set()
        for word in spam_words:
            if word.isalnum() and len(word) > 12:
                word_set.add(word)
            if word.startswith('<') or word.endswith('!'):
                word_set.add(word)
        return word_set

    def test(self, test_corpus_dir):
        emails = os.listdir(test_corpus_dir)
        with open(test_corpus_dir + "\\!prediction.txt", 'wt', encoding='utf-8') as f:
            for email in emails:
                if not email.startswith("!"):
                    result = self.skim_mail(test_corpus_dir, email)
                    f.write(email + ' ' + result + '\n')
        print(f"Word count: {len(self.spam_words)}")

    def skim_mail(self, test_corpus_dir, email):
        keywords = \
            ["Save", "save", "Discount", "discount", "Sale", "sale",
             "Spend", "spend", "Insurance", "Buy", "buy",
             "<table>", "<html>", "<body>", "href", "<br>", "<p>"]
        spam_words = self.spam_words

        with open(test_corpus_dir + "\\" + email, "rt", encoding='utf-8') as f:
            text = f.read()
            words = text.split()
            keyword_counter = 0
            if len(text) > 12000:
                return "SPAM"

            for keyword in spam_words:
                keyword_counter += text.count(keyword)
                if keyword_counter > 6:
                    return "SPAM"

            allcase_count = 0
            for word in words:
                if word.isupper() and word.isalpha():
                    allcase_count += 1
                if allcase_count > 60:
                    return "SPAM"

            return "OK"


if __name__ == "__main__":
    filter = MyFilter()
    filter.train(os.getcwd() + "\\spam-data-12-s75-h25\\2")
    filter.test(os.getcwd() + "\\spam-data-12-s75-h25\\1")

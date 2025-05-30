class BinaryConfusionMatrix:
    """Confusion Matrix for our Spam filter"""

    def __init__(self, pos_tag, neg_tag):
        self.pos_tag = pos_tag  # True
        self.neg_tag = neg_tag  # False
        self.tp = 0
        self.tn = 0
        self.fp = 0
        self.fn = 0

    def as_dict(self):
        cmdict = {'tp': self.tp, 'tn': self.tn, 'fp': self.fp, 'fn': self.fn}
        return cmdict

    def update(self, truth, pred):
        if truth != self.pos_tag and truth != self.neg_tag or \
                pred != self.pos_tag and pred != self.neg_tag:
            raise ValueError
        else:
            if truth == self.pos_tag and pred == self.pos_tag:
                self.tp += 1
            elif truth == self.neg_tag and pred == self.neg_tag:
                self.tn += 1
            elif truth == self.neg_tag and pred == self.pos_tag:
                self.fp += 1
            elif truth == self.pos_tag and pred == self.neg_tag:
                self.fn += 1

    def compute_from_dicts(self, truth_dict, pred_dict):
        t = sorted(truth_dict.items())
        p = sorted(pred_dict.items())
        for i in range(len(t)):
            self.update(t[i][1], p[i][1])

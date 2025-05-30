import unittest
from confmat import BinaryConfusionMatrix

SPAM_TAG = "SPAM"
HAM_TAG = "OK"


class BinaryConfusionMatrixTest(unittest.TestCase):

    def setUp(self):
        self.cm = BinaryConfusionMatrix(pos_tag=SPAM_TAG, neg_tag=HAM_TAG)

    def test_countersAreZero_afterCreation(self):
        cm_dict = self.cm.as_dict()
        self.assertDictEqual(cm_dict, {'tp': 0, 'tn': 0, 'fp': 0, 'fn': 0})

    def test_updatesTPcorrectly(self):
        self.cm.update(SPAM_TAG, SPAM_TAG)
        self.assertDictEqual(self.cm.as_dict(), {'tp': 1, 'tn': 0, 'fp': 0, 'fn': 0})

    def test_updatesTNcorrectly(self):
        self.cm.update(HAM_TAG, HAM_TAG)
        self.assertDictEqual(self.cm.as_dict(), {'tp': 0, 'tn': 1, 'fp': 0, 'fn': 0})

    def test_updatesFPcorrectly(self):
        self.cm.update(HAM_TAG, SPAM_TAG)
        self.assertDictEqual(self.cm.as_dict(), {'tp': 0, 'tn': 0, 'fp': 1, 'fn': 0})

    def test_updatesFNcorrectly(self):
        self.cm.update(SPAM_TAG, HAM_TAG)
        self.assertDictEqual(self.cm.as_dict(), {'tp': 0, 'tn': 0, 'fp': 0, 'fn': 1})

    def test_update_raisesValueError_forWrongTruthValue(self):
        with self.assertRaises(ValueError):
            self.cm.update('a bad value', SPAM_TAG)

    def test_update_raisesValueError_forWrongPredictionValue(self):
        with self.assertRaises(ValueError):
            self.cm.update(SPAM_TAG, 'a bad value')

    def test_computerFromDicts_allCasesOnce(self):
        truth = {1: SPAM_TAG, 2: SPAM_TAG, 3: HAM_TAG, 4: HAM_TAG}
        predict = {1: SPAM_TAG, 2:HAM_TAG, 3: SPAM_TAG, 4: HAM_TAG}
        self.cm.compute_from_dicts(truth, predict)
        self.assertDictEqual(self.cm.as_dict(), {'tp':1,'tn':1,'fp':1,'fn':1})


if __name__ == '__main__':
    unittest.main()

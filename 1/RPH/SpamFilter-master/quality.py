import utils
import confmat
import os


def quality_score(tp, tn, fp, fn):
    score = (tp + tn) / (tp + tn + 10 * fp + fn)
    return score


def compute_quality_for_corpus(corpus_dir):
    truth = utils.read_classification_from_file(corpus_dir + '/!truth.txt')
    prediction = utils.read_classification_from_file(corpus_dir + '/!prediction.txt')
    matrix = confmat.BinaryConfusionMatrix('SPAM', 'OK')
    d = matrix.compute_from_dicts(truth, prediction)
    tp = d['tp']
    tn = d['tn']
    fp = d['fp']
    fn = d['fn']
    quality = quality_score(tp, tn, fp, fn)
    print(f"TP: {tp}\nTN: {tn}\nFP: {fp}\nFN: {fn}")
    print(quality)
    return quality


if __name__ == "__main__":
    """needs  added"""
    compute_quality_for_corpus(os.getcwd() + '/spam-data-12-s75-h25/1')

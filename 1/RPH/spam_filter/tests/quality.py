import os
from utils import read_classification_from_file
from confmat import BinaryConfusionMatrix


def quality_score(tp, tn, fp, fn):
    return (tp + tn) / (tp + tn + 10 * fp + fn)


def compute_quality_for_corpus(corpus_dir):
    entries = os.listdir(corpus_dir)
    """list of file names"""
    truth_path = os.path.join(corpus_dir, entries[1])
    """path to !truth.txt"""
    pred_path = os.path.join(corpus_dir, entries[0])
    """path to !prediction.txt"""
    truth_dict = read_classification_from_file(truth_path)
    """dictionary of emails and tags from !truth.txt"""
    pred_dict = read_classification_from_file(pred_path)
    """dictionary of emails and tags from !prediction.txt"""
    b = BinaryConfusionMatrix(pos_tag='SPAM', neg_tag='OK')
    """BinaryConfusionMatrix class and tags"""
    BinaryConfusionMatrix.compute_from_dicts(b, truth_dict, pred_dict)
    """calculation of confusion matrix: tp, tn, fp, fn"""
    vals = list(BinaryConfusionMatrix.as_dict(b).values())
    """list of updated tp, tn, fp, fn values"""
    return quality_score(vals[0], vals[1], vals[2], vals[3])

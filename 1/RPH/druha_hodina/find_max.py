def find_max(in_list):
    """
    find maximum value in a list or tuple
    :param in_list: a list or tuple of elements that allow < comparison
    :return: a tuple max_value, index of the max_value within the list
    """
    curr_max = in_list[0]
    curr_indx = 0
    for i in range(1, len(in_list)):
        if curr_max < in_list[i]:
            curr_max = in_list[i]
            curr_indx = i
    return curr_max, curr_indx

def find_max_better(vec):
    if not(vec):    # if len(vec) == 0:
        return None
    else:
        return find_max(vec)

if __name__ == "__main__":
    seznam = [-10,-20,-3,-5]
    print(find_max(seznam))
    vec = []
    #print(find_max(vec))
    print(find_max_better(vec))
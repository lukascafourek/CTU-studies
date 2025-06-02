import random

def print_data(data):
    for row in data:
        print(row)

def generate_data(n_cols, n_rows):
    arr = []
    for i in range(n_rows):
        n_col = []
        for j in range(n_cols):
            x = random.randint(0, 1)
            n_col.append(x)
        arr.append(n_col)
    return arr

def is_inside(r, c, data):
    return (r>=0) and (c>=0) and (r<len(data)) and (c<len(data[r]))

def search_dir(dir, r, c, data):
    len = 0
    r1 = r + dir[0]
    c1 = c + dir[1]
    #print(r1, c1)
    while (is_inside(r1, c1, data)):
        if data[r1][c1] == data[r][c]:
            len += 1
            r1 += dir[0]
            c1 += dir[1]
            #print(r1, c1, ": ", len)
        else:
            break
    return len

def line_size(r , c, data):
    dirs = [(0,1), (0,-1)]
    l_size = 1
    for dir in dirs:
        l_size += search_dir(dir, r, c, data)
    #print(l_size)
    return l_size

def line_column_size(r, c, data):
    dirs = [(0,1), (0,-1), (1,0), (-1,0)]
    l_c_size = 1
    for dir in dirs:
        l_c_size += search_dir(dir, r, c, data)
    #print(l_c_size)
    return l_c_size

def region_size(r, c, data):
    dirs = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]
    reg_size = 1
    for dir in dirs:
        reg_size += search_dir(dir, r, c, data)
    #print(reg_size)
    return(reg_size)


if __name__ == '__main__':
    data = generate_data(8, 8)
    print_data(data)
    r = 5
    c = 5
    reg_size = line_size(r, c, data)
    print(reg_size)
    reg_size2 = line_column_size(r, c, data)
    print(reg_size2)
    reg_size3 = region_size(r, c, data)
    print(reg_size3)

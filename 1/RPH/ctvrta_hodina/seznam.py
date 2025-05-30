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

def is_inside(r1, c1, r2, c2, r3, c3, r4, c4, r5, c5, r6, c6):
    if 0 <= r1 < 8 or 0 <= c1 < 8 or 0 <= r2 < 8 or 0 <= c2 < 8 or \
            0 <= r3 < 8 or 0 <= c3 < 8 or 0 <= r4 < 8 or 0 <= c4 < 8 or \
            0 <= r5 < 8 or 0 <= c5 < 8 or 0 <= r6 < 8 or 0 <= c6 < 8:
        return True
    else:
        return False

def line_size(r, c, data):
    count = 1
    c1 = c + 1
    c2 = c - 1
    r1 = r2 = r3 = r4 = r5 = r6 = r
    c3 = c4 = c5 = c6 = c
    while is_inside(r1, c1, r2, c2, r3, c3, r4, c4, r5, c5, r6, c6):
        if 0 <= c1 < 8 and data[r][c] == data[r][c1]:
            count += 1
            c1 += 1
        elif 0 <= c2 < 8 and data[r][c] == data[r][c2]:
            count += 1
            c2 -= 1
        else:
            break
    return count

def line_column_size(r, c, data):
    count = 1
    c1 = c + 1
    c2 = c - 1
    r1 = r + 1
    r2 = r - 1
    r3 = r4 = r5 = r6 = r
    c3 = c4 = c5 = c6 = c
    while is_inside(r1, c1, r2, c2, r3, c3, r4, c4, r5, c5, r6, c6):
        if 0 <= c1 < 8 and data[r][c] == data[r][c1]:
            count += 1
            c1 += 1
        elif 0 <= c2 < 8 and data[r][c] == data[r][c2]:
            count += 1
            c2 -= 1
        elif 0 <= r1 < 8 and data[r][c] == data[r1][c]:
            count += 1
            r1 += 1
        elif 0 <= r2 < 8 and data[r][c] == data[r2][c]:
            count += 1
            r2 -= 1
        else:
            break
    return count

def region_size(r, c, data):
    count = 1
    c1 = c3 = c5 = c + 1
    c2 = c4 = c6 = c - 1
    r1 = r3 = r5 = r + 1
    r2 = r4 = r6 = r - 1
    while is_inside(r1, c1, r2, c2, r3, c3, r4, c4, r5, c5, r6, c6):
        if 0 <= c1 < 8 and data[r][c] == data[r][c1]:
            count += 1
            c1 += 1
        elif 0 <= c2 < 8 and data[r][c] == data[r][c2]:
            count += 1
            c2 -= 1
        elif 0 <= r1 < 8 and data[r][c] == data[r1][c]:
            count += 1
            r1 += 1
        elif 0 <= r2 < 8 and data[r][c] == data[r2][c]:
            count += 1
            r2 -= 1
        elif 0 <= r3 < 8 and 0 <= c3 < 8 and data[r][c] == data[r3][c3]:
            count += 1
            r3 += 1
            c3 += 1
        elif 0 <= r4 < 8 and 0 <= c4 < 8 and data[r][c] == data[r4][c4]:
            count += 1
            r4 -= 1
            c4 -= 1
        elif 0 <= r5 < 8 and 0 <= c6 < 8 and data[r][c] == data[r5][c6]:
            count += 1
            r5 += 1
            c6 -= 1
        elif 0 <= r6 < 8 and 0 <= c5 < 8 and data[r][c] == data[r6][c5]:
            count += 1
            r6 -= 1
            c5 += 1
        else:
            break
    return count

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

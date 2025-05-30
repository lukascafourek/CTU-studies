def value_count(data, value):
    count = 0
    for row in data:
        for col in row:
            if col == value:
                count += 1
    return count

def value_positions(data, value):
    positions = []
    for i, row in enumerate(data):
        for j, col in enumerate(row):
            if col == value:
                positions.append((i, j))
    return positions

if __name__ == '__main__':
    value = -1
    data = [[0, -1, 1], [-1, 0, -1], [1, 0, -1]]
    count = value_count(data, value)
    print(count)
    positions = value_positions(data, value)
    print(positions)

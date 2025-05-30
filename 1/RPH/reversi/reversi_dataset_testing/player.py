import random


class MyPlayer:
    """This player chooses and plays greedy moves"""

    def __init__(self, my_color, opponent_color):
        self.name = 'cafoulu1'
        self.my_color = my_color
        self.opponent_color = opponent_color

    def move(self, board):
        valid_moves = []
        counts = []
        dirs = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
        for r in range(len(board)):
            for c in range(len(board[r])):
                if board[r][c] == -1:
                    self.search_moves(board, r, c, dirs, valid_moves, counts)
        if valid_moves and counts:
            index = self.search_greedy_move(counts)
            return valid_moves[index]

    def is_inside(self, r, c, board):
        return (r >= 0) and (c >= 0) and (r < len(board)) and (c < len(board[r]))

    def search_dir(self, dir, r, c, board, counts):
        count = 1
        r1 = r + dir[0]
        c1 = c + dir[1]
        while self.is_inside(r1, c1, board):
            if board[r1][c1] == self.opponent_color:
                r1 += dir[0]
                c1 += dir[1]
                while self.is_inside(r1, c1, board):
                    if board[r1][c1] == self.my_color:
                        counts.append(count)
                        return r, c
                    elif board[r1][c1] == self.opponent_color:
                        count += 1
                        break
                    else:
                        break
            else:
                break

    def search_moves(self, board, r, c, dirs, valid_moves, counts):
        for dir in dirs:
            valid_move = self.search_dir(dir, r, c, board, counts)
            if valid_move:
                valid_moves.append(valid_move)

    def search_greedy_move(self, counts):
        indexes = []
        maximum = max(counts)
        for i in range(len(counts)):
            if counts[i] == maximum:
                indexes.append(i)
        if indexes:
            index = random.choice(indexes)
            return index


if __name__ == "__main__":
    board = [
        [-1, -1, -1, -1, -1, -1, -1, -1],
        [-1, -1, -1, -1, -1, -1, -1, -1],
        [-1, -1, -1, -1, -1, -1, -1, -1],
        [-1, -1, -1, 0, 1, -1, -1, -1],
        [-1, -1, -1, 1, 0, -1, -1, -1],
        [-1, -1, -1, -1, -1, -1, -1, -1],
        [-1, -1, -1, -1, -1, -1, -1, -1],
        [-1, -1, -1, -1, -1, -1, -1, -1]]
    p1 = MyPlayer(0, 1)
    p1_move = (p1.move(board))
    p2 = MyPlayer(1, 0)
    p2_move = (p2.move(board))
    print(p1_move)
    print(p2_move)

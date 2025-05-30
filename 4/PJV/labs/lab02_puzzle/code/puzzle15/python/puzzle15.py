UP = 1
DOWN = 2
LEFT = 3
RIGHT = 4

board = [
  [ 1, 2, 3, 4],
  [ 5, 6, 7, 8],
  [ 9,10,11,12],
  [13,14,15, 0],
]

def move_tile(direction):
  for r in range(len(board)):
    for c in range(len(board[0])):
      if board[r][c] == 0:
        # DRY?
        if direction == UP:
          if r < len(board) - 1:
            board[r][c] = board[r + 1][c]
            board[r + 1][c] = 0
        elif direction == DOWN:
          if r > 0:
            board[r][c] = board[r - 1][c]
            board[r - 1][c] = 0
        elif direction == LEFT:
          if c < len(board[0]) - 1:
            board[r][c] = board[r][c + 1]
            board[r][c + 1] = 0
        elif direction == RIGHT:
          if c > 0:
            board[r][c] = board[r][c - 1]
            board[r][c - 1] = 0
        return

def print_line(length):
  for i in range (length):
    print("-", end = "")
  print("")

def print_board():
  print_line(5 * len(board[0]) + 1)
  for row in board:
    for col in row:
      if col > 0:
        print(f"| {col:2} ", end = "")
      else:
        print("|    ", end = "")
    print("|")
    print_line(5 * len(board[0]) + 1)

# TEST

print_board()
move_tile(RIGHT)
move_tile(UP)
move_tile(DOWN)
move_tile(LEFT)
print_board()
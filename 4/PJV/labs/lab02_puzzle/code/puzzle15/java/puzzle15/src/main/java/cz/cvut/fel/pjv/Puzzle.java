package cz.cvut.fel.pjv;

public class Puzzle {

  private final int[][] board = {
    {1, 2, 3, 4},
    {5, 6, 7, 8},
    {9, 10, 11, 12},
    {13, 14, 15, 0}
  };

  public final static int UP = 1;
  public final static int DOWN = 2;
  public final static int LEFT = 3;
  public final static int RIGHT = 4;
  
  public void move(int direction) {
    for (int r = 0; r < board.length; r++) {
      for (int c = 0; c < board[0].length; c++) {
        if (board[r][c] == 0) {
          switch (direction) {
            case UP:
              if (r < board.length - 1) {
                board[r][c] = board[r + 1][c];
                board[r + 1][c] = 0;
              }
              break;
            case DOWN:
              if (r > 0) {
                board[r][c] = board[r - 1][c];
                board[r - 1][c] = 0;
              }
              break;
            case LEFT:
              if (c < board[0].length - 1) {
                board[r][c] = board[r][c + 1];
                board[r][c + 1] = 0;
              }
              break;
            case RIGHT:
              if (c > 0) {
                board[r][c] = board[r][c - 1];
                board[r][c - 1] = 0;
              }
              break;
          }
          return;
        }
      }
    }
  }

  private void printLine(int length) {
    for (int i = 0; i < length; i++) {
      System.out.print("-");
    }
    System.out.println();
  }
  
  public void printBoard() {
    printLine(5 * board.length + 1);
    for (int[] row : board) {
      for (int tile : row) {
        if (tile > 0) {
          System.out.printf("| %2d ", tile);
        } else {
          System.out.print("|    ");
        }
      }
      System.out.println("|");
      printLine(5 * board.length + 1);
    }
  }
}

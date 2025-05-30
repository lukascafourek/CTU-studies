package cz.cvut.fel.pjv.puzzle15fx;

public class Model {

  private int[][] board = {
    {1, 2, 3, 4},
    {5, 6, 7, 8},
    {9, 10, 11, 12},
    {13, 14, 15, 0}
  };
  
  public final static int UP = 1;
  public final static int DOWN = 2;
  public final static int LEFT = 3;
  public final static int RIGHT = 4;


  public int getRowCount() {
    return board.length;
  }

  public int getColumnCount() {
    return board[0].length;
  }

  public int[][] getBoard() {
  // dont't let others to modify board - return a copy!
    int[][] safeCopy = new int[getRowCount()][getColumnCount()];
    for (int col = 0; col < getColumnCount(); col++) {
      for (int row = 0; row < getRowCount(); row++) {
        safeCopy[row][col] = board[row][col];
      }
    }
    return safeCopy;
  }
  
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
  
}

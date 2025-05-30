package cz.cvut.fel.pjv;

public class Start {

  public static void main(String[] args) {
    
    Puzzle puzzle = new Puzzle();
    
    puzzle.printBoard();

    puzzle.move(Puzzle.RIGHT);
    puzzle.move(Puzzle.UP);
    puzzle.move(Puzzle.DOWN);
    puzzle.move(Puzzle.LEFT);
    
    puzzle.printBoard();
  }
}

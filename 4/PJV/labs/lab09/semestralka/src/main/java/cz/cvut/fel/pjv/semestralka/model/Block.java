package cz.cvut.fel.pjv.semestralka.model;

public class Block {
  private int row, column;

  public Block(int row, int column) {
    this.row = row;
    this.column = column;
  }

  public Block() {
  }

  public int getRow() {
    return row;
  }

  public void setRow(int row) {
    this.row = row;
  }

  public int getColumn() {
    return column;
  }

  public void setColumn(int column) {
    this.column = column;
  }

  public void incrementRow() {
    row++;
  }

  public void incrementColumn() {
    column++;
  }

  public void decrementColumn() {
    column--;
  }
}

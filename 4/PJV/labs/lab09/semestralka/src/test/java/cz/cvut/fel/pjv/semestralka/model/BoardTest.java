package cz.cvut.fel.pjv.semestralka.model;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class BoardTest {

  @Test
  void testCreateBlockSize() {
    Board sut = new Board();
    sut.createBlock();
    int expected = 1;
    int actual = sut.getBlocks().size();
    assertEquals(expected, actual);
  }

  @Test
  void testCreateBlockRow() {
    Board sut = new Board();
    sut.createBlock();
    int expected = 0;
    int actual = sut.getBlocks().getLast().getRow();
    assertEquals(expected, actual);
  }

  @Test
  void testFullRow() {
    Board sut = new Board();
    List<Block> blocks = sut.getBlocks();
    int columnCount = sut.getWidth() / sut.getTileDim();
    int lastRow = sut.getHeight() / sut.getTileDim() - 1;
    for (int column = 0; column < columnCount; column++) {
      blocks.add(new Block(lastRow, column));
    }
    for (int i = 0; i < 11; i++) {
      sut.update();
    }
    int expected = 1;
    int actual = sut.getBlocks().size();
    assertEquals(expected, actual);
  }

  @Test
  void testNotFullRow() {
    Board sut = new Board();
    List<Block> blocks = sut.getBlocks();
    int columnCount = sut.getWidth() / sut.getTileDim();
    int lastRow = sut.getHeight() / sut.getTileDim() - 1;
    for (int column = 0; column < columnCount -1; column++) {
      blocks.add(new Block(lastRow, column));
    }
    for (int i = 0; i < 11; i++) {
      sut.update();
    }
    int expected = columnCount;
    int actual = sut.getBlocks().size();
    assertEquals(expected, actual);
  }

  @Test
  void testNotFullRowLast() {
    Board sut = new Board();
    List<Block> blocks = sut.getBlocks();
    int columnCount = sut.getWidth() / sut.getTileDim();
    int lastRow = sut.getHeight() / sut.getTileDim() - 1;
    for (int column = 0; column < columnCount -1; column++) {
      blocks.add(new Block(lastRow, column));
    }
    for (int i = 0; i < 11; i++) {
      sut.update();
    }
    int expected = 0;
    int actual = sut.getBlocks().getLast().getRow();
    assertEquals(expected, actual);
  }


}
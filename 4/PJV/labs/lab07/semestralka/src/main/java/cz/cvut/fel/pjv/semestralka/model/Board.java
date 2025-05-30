package cz.cvut.fel.pjv.semestralka.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import cz.cvut.fel.pjv.semestralka.Constants;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Board {
  private final int TILE_DIM = 64; // pixels
  private final int ROW_CNT = 14;
  private final int COLUMN_CNT = 8;
  private List<Block> blocks = new ArrayList<>();
  private final Random rnd = new Random();
  private BlockDirection currentDirection = BlockDirection.NONE;
  private int counter = 0;

  private final static System.Logger logger = System.getLogger(Board.class.getName());

  public void createBlock() {
    blocks.add(new Block(0, rnd.nextInt(COLUMN_CNT)));
  }

  public int getWidth() {
    return COLUMN_CNT * TILE_DIM;
  }

  public int getHeight() {
    return ROW_CNT * TILE_DIM;
  }

  public int getTiteDim() {
    return TILE_DIM;
  }

  public List<Block> getBlocks() {
    return blocks;
  }

  public void setCurrentDirection(BlockDirection currentDirection) {
    this.currentDirection = currentDirection;
  }

  public void update() {
    logger.log(System.Logger.Level.TRACE, "Model will be updated.");
    Block activeBlock = blocks.getLast();
    if (counter == 10) {
      counter = 0;
      if (activeBlock.getRow() < ROW_CNT - 1) {
        activeBlock.incrementRow();
      } else {
        createBlock();
      }
    }
    counter ++;
    if (currentDirection == BlockDirection.RIGHT &&
        activeBlock.getColumn() < COLUMN_CNT - 1) {
      activeBlock.incrementColumn();
    } else if (currentDirection == BlockDirection.LEFT &&
        activeBlock.getColumn() > 0) {
      activeBlock.decrementColumn();
    }
  }

  public void saveGame() {
    GameData gameData = new GameData();
    gameData.blocks = blocks;
    ObjectMapper objectMapper = new ObjectMapper();
    try {
      objectMapper.writeValue(new File(Constants.saveGameFileName), gameData);
    } catch (IOException e) {
      System.err.println("Can't save game: " + e.getMessage());
    }
  }

  public boolean loadGame() {
    ObjectMapper objectMapper = new ObjectMapper();
    try {
      GameData gameData  = objectMapper.readValue(new File(Constants.saveGameFileName), GameData.class);
      blocks = gameData.blocks;
      if (blocks.isEmpty()) {
        return false;
      }
      return true;
    } catch (IOException e) {
      System.err.println("Can't load game: " + e.getMessage());
      return false;
    }
  }
}

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
  private GameState gameState = GameState.PAUSED;

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

  public int getTileDim() {
    return TILE_DIM;
  }

  public List<Block> getBlocks() {
    return blocks;
  }

  public GameState getGameState() {
    return gameState;
  }

  public void setGameState(GameState gameState) {
    this.gameState = gameState;
  }

  public void setCurrentDirection(BlockDirection currentDirection) {
    this.currentDirection = currentDirection;
  }

  public void update() {
    logger.log(System.Logger.Level.TRACE, "Model will be updated.");
    Block activeBlock = blocks.getLast();
    if (counter == 10) {
      counter = 0;
      if (!isOnGround(activeBlock) && !isBlockUnder(activeBlock)) {
        activeBlock.incrementRow();
      } else {
        // landing
        int landingRow = activeBlock.getRow();
        if (isFullRow(landingRow)) {
          logger.log(System.Logger.Level.INFO, "Deleting row..");
          deleteRow(landingRow);
          fallDownAbove(landingRow);
        }
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

  private boolean isOnGround(Block block) {
    return block.getRow() == ROW_CNT - 1;
  }

  private boolean isBlockUnder(Block block) {
    for (Block block_i : blocks) {
      if (block_i.getRow() == block.getRow()+1 &&
          block_i.getColumn() == block.getColumn()){
        return true;
      }
    }
    return false;
  }

  private boolean isFullRow(int row) {
    int currentBlockAmountInRow = 0;
    for (Block block : blocks) {
      if (block.getRow() == row) {
        currentBlockAmountInRow++;
        if (currentBlockAmountInRow == COLUMN_CNT) return true;
      }
    }
    return false;
  }

  private void deleteRow(int row) {
    blocks.removeIf(block -> block.getRow() == row);
  }

  private void fallDownAbove(int row) {
    for (Block block : blocks) {
      if (block.getRow() < row) {
        block.incrementRow();
      }
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

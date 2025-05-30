package cz.cvut.fel.pjv.semestralka;

import cz.cvut.fel.pjv.semestralka.model.Block;
import cz.cvut.fel.pjv.semestralka.model.BlockDirection;
import cz.cvut.fel.pjv.semestralka.model.Board;
import cz.cvut.fel.pjv.semestralka.model.GameState;
import javafx.animation.AnimationTimer;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

public class GameScene extends Scene {
  private final Image backgroundImage;
  private final Image blockImage;
  private final Board board;
  private Scene mainMenuScene;
  public GameScene(Board board, Stage stage) {
    super(new StackPane(), board.getWidth(), board.getHeight());
    Canvas canvas = new Canvas(board.getWidth(), board.getHeight());
    Pane pane = (Pane) getRoot();
    pane.getChildren().add(canvas);

    this.board = board;

    backgroundImage = new Image(Constants.backgroundImgFileName);
    blockImage = new Image(Constants.blockImgFileName);

    // FTB
    if (!board.loadGame()) {
      board.createBlock();
    }

    setOnKeyPressed(new EventHandler<KeyEvent>() {
      @Override
      public void handle(KeyEvent keyEvent) {
        if (board.getGameState() == GameState.RUNNING) {
          if (keyEvent.getCode() == KeyCode.LEFT) {
            board.setCurrentDirection(BlockDirection.LEFT);
          } else if (keyEvent.getCode() == KeyCode.RIGHT) {
            board.setCurrentDirection(BlockDirection.RIGHT);
          }
        }
        if (keyEvent.getCode() == KeyCode.ESCAPE) {
          board.setGameState(board.getGameState() == GameState.RUNNING ? GameState.PAUSED : GameState.RUNNING);
        } else if (keyEvent.getCode() == KeyCode.M) {
          if (mainMenuScene != null) {
            board.setGameState(GameState.PAUSED);
            stage.setScene(mainMenuScene);
          }
        }
      }
    });

    setOnKeyReleased(new EventHandler<KeyEvent>() {
      @Override
      public void handle(KeyEvent keyEvent) {
        if (board.getGameState() == GameState.RUNNING) {
          if (keyEvent.getCode() == KeyCode.LEFT ||
              keyEvent.getCode() == KeyCode.RIGHT) {
            board.setCurrentDirection(BlockDirection.NONE);
          }
        }
      }
    });

    AnimationTimer timer = new AnimationTimer() {
      long lastCall;
      @Override
      public void handle(long l) {
        if ((l - lastCall) >= 50_000_000) {
          if (board.getGameState() == GameState.RUNNING) {
            board.update();
            render(canvas);
          }
          lastCall = l;
        }
      }
    };
    timer.start();
  }

  private void render(Canvas canvas) {
    renderBoard(canvas);
    renderBlocks(canvas);
  }

  private void renderBoard(Canvas canvas) {
    GraphicsContext gc = canvas.getGraphicsContext2D();
    gc.drawImage(backgroundImage, 0, 0);
    gc.setStroke(Color.rgb(255,255,255,0.5));
    for (int i = 0; i <= board.getWidth(); i += board.getTileDim()) {
      gc.strokeLine(i, 0, i, board.getHeight());
    }
    for (int i = 0; i <= board.getHeight(); i += board.getTileDim()) {
      gc.strokeLine(0, i, board.getWidth(), i);
    }
  }

  private void renderBlocks(Canvas canvas) {
    GraphicsContext gc = canvas.getGraphicsContext2D();
    for (Block block : board.getBlocks()) {
      gc.drawImage(blockImage,
          block.getColumn() * board.getTileDim(),
          block.getRow() * board.getTileDim());
    }
  }

  public void setMainMenuScene(Scene mainMenuScene) {
    this.mainMenuScene = mainMenuScene;
  }
}

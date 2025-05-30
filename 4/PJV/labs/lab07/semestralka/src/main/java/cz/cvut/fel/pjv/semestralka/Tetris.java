package cz.cvut.fel.pjv.semestralka;

import cz.cvut.fel.pjv.semestralka.model.BlockDirection;
import cz.cvut.fel.pjv.semestralka.model.Board;
import cz.cvut.fel.pjv.semestralka.model.Block;

import javafx.animation.AnimationTimer;
import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.layout.Pane;

public class Tetris extends Application {
    private Board board;
    private Image backgroundImage;
    private Image blockImage;

    @Override
    public void start(Stage stage) {
        board = new Board();
        backgroundImage = new Image(Constants.backgroundImgFileName);
        blockImage = new Image(Constants.blockImgFileName);

        Canvas canvas = new Canvas(board.getWidth(), board.getHeight());

        // FTB
        if (!board.loadGame()) {
            board.createBlock();
        }

        Pane pane = new StackPane(canvas);
        Scene scene = new Scene(pane, board.getWidth(), board.getHeight());

        scene.setOnKeyPressed(new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent keyEvent) {
                if (keyEvent.getCode() == KeyCode.LEFT) {
                    board.setCurrentDirection(BlockDirection.LEFT);
                } else if (keyEvent.getCode() == KeyCode.RIGHT) {
                    board.setCurrentDirection(BlockDirection.RIGHT);
                }
            }
        });

        scene.setOnKeyReleased(new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent keyEvent) {
                if (keyEvent.getCode() == KeyCode.LEFT ||
                    keyEvent.getCode() == KeyCode.RIGHT) {
                    board.setCurrentDirection(BlockDirection.NONE);
                }
            }
        });

        AnimationTimer timer = new AnimationTimer() {
            long lastCall;
            @Override
            public void handle(long l) {
                if ((l - lastCall) >= 50_000_000) {
                    board.update();
                    render(canvas);
                    lastCall = l;
                }
            }
        };
        timer.start();

        stage.setTitle("Tetris");
        stage.setResizable(false);
        stage.setScene(scene);
        stage.show();
    }

    @Override
    public void stop() throws Exception {
        super.stop();
        board.saveGame();
    }

    private void render(Canvas canvas) {
        renderBoard(canvas);
        renderBlocks(canvas);
    }

    private void renderBoard(Canvas canvas) {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.drawImage(backgroundImage, 0, 0);
        gc.setStroke(Color.rgb(255,255,255,0.5));
        for (int i = 0; i <= board.getWidth(); i += board.getTiteDim()) {
            gc.strokeLine(i, 0, i, board.getHeight());
        }
        for (int i = 0; i <= board.getHeight(); i += board.getTiteDim()) {
            gc.strokeLine(0, i, board.getWidth(), i);
        }
    }

    private void renderBlocks(Canvas canvas) {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        for (Block block : board.getBlocks()) {
            gc.drawImage(blockImage,
                block.getColumn() * board.getTiteDim(),
                block.getRow() * board.getTiteDim());
        }
    }

    public static void main(String[] args) {
        launch();
    }
}
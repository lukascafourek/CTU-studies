package cz.cvut.fel.pjv.semestralka;

import cz.cvut.fel.pjv.semestralka.model.Board;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class Tetris extends Application {
    private Board board;

    @Override
    public void start(Stage stage) {
        board = new Board();

        GameScene gameScene = new GameScene(board, stage);
        Scene mainMenuScene = new MainMenuScene(board, stage, gameScene);
        gameScene.setMainMenuScene(mainMenuScene);

        stage.setTitle("Tetris");
        stage.setResizable(false);
        stage.setScene(mainMenuScene);
        stage.show();
    }

    @Override
    public void stop() throws Exception {
        super.stop();
        board.saveGame();
    }

    public static void main(String[] args) {
        launch();
    }
}
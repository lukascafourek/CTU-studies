package cz.cvut.fel.pjv.semestralka;

import cz.cvut.fel.pjv.semestralka.model.Board;
import cz.cvut.fel.pjv.semestralka.model.GameState;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

public class MainMenuScene extends Scene {
  private Board board;
  public MainMenuScene(Board board, Stage stage, Scene gameScene) {
    super(new VBox(), board.getWidth(), board.getHeight());
    VBox root = (VBox) getRoot();

    Button startButton = new Button("Start Game");
    startButton.setOnAction(new EventHandler<ActionEvent>() {
      @Override
      public void handle(ActionEvent actionEvent) {
        stage.setScene(gameScene);
        board.setGameState(GameState.RUNNING);
      }
    });
    Button dummyButton1 = new Button("Dummy 1");
    Button dummyButton2 = new Button("Dummy 2");

    root.getChildren().addAll(startButton, dummyButton1, dummyButton2);
    root.setAlignment(Pos.CENTER);
    root.setSpacing(20);

    this.board = board;
  }
}

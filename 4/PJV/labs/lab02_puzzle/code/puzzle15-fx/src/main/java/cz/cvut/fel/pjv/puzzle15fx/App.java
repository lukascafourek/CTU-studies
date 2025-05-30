package cz.cvut.fel.pjv.puzzle15fx;

import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.geometry.VPos;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.TextAlignment;
import javafx.stage.Stage;

public class App extends Application {
    private final static int cellDim = 100; // in pixels
    private Model model;

    @Override
    public void start(Stage stage) {
      // stav hry - MODEL
      model = new Model();
      
      stage.setTitle("Puzzle 15");
      var canvas = new Canvas(model.getRowCount() * cellDim, model.getColumnCount() * cellDim);
      var gc = canvas.getGraphicsContext2D();
      var scene = new Scene(new Pane(canvas));

      // uzivatelsky vstup - CONTROLLER
      scene.setOnKeyPressed(new EventHandler<KeyEvent>() {
        @Override
        public void handle(KeyEvent keyEvent) {
          switch (keyEvent.getCode()) {
            case UP:
              model.move(Model.UP);
              break;
            case DOWN:
              model.move(Model.DOWN);
              break;
            case LEFT:
              model.move(Model.LEFT);
              break;
            case RIGHT:
              model.move(Model.RIGHT);
              break;
          }
          paintUi(gc, canvas.getWidth(), canvas.getHeight());
        }
      });

      stage.setScene(scene);
      paintUi(gc, canvas.getWidth(), canvas.getHeight());
      stage.show();
    }

    // kresleni - VIEW
    private void paintUi(GraphicsContext gc, double width, double height) {
      int[][] nums = model.getBoard();
      
      gc.clearRect(0, 0, width, height);
      gc.setStroke(Color.rgb(0x86, 0xB0, 0x30));
      gc.setTextAlign(TextAlignment.CENTER);
      gc.setTextBaseline(VPos.CENTER);
      gc.setFont(new Font("Verdana Bold", cellDim / 2.5));

      for (int row = 0; row < model.getRowCount(); row++) {
        for (int col = 0; col < model.getColumnCount(); col++) {
          if (nums[row][col] > 0) {
            gc.setFill(Color.rgb(0xEC, 0xF5, 0xD9));
            gc.fillRoundRect(col * cellDim, row * cellDim, cellDim, cellDim, cellDim / 5, cellDim / 5);
            gc.strokeRoundRect(col * cellDim, row * cellDim, cellDim, cellDim, cellDim / 5, cellDim / 5);
            gc.setFill(Color.rgb(0x86, 0xB0, 0x30));
            gc.fillText(String.valueOf(nums[row][col]), (col + 0.5) * cellDim, (row + 0.5) * cellDim);
          }
        }
      }
    }

    public static void main(String[] args) {
        launch();
    }
}
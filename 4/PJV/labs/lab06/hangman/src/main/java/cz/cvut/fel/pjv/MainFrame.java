package cz.cvut.fel.pjv;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;

public class MainFrame extends JFrame implements ActionListener {
  private final Model model = new Model();
  private final JLabel imageLabel;
  private final JLabel guessedWordLabel;

  public MainFrame() throws HeadlessException {
    model.initGame(new RandomWordSource());

    setTitle("Hangman");
    setSize(600,600);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    Container cp = getContentPane();
    cp.setLayout(new BoxLayout(cp, BoxLayout.Y_AXIS));

    imageLabel = new JLabel();
    updateImageLabel(model.getMissCount());
    JPanel upperPart = new JPanel();
    upperPart.add(imageLabel);

    guessedWordLabel = new JLabel(model.getGuessedWord());
    guessedWordLabel.setFont(guessedWordLabel.getFont().deriveFont(64.0f));
    JPanel centerPart = new JPanel();
    centerPart.add(guessedWordLabel);

    JPanel lowerPart = new JPanel();
    for (char c : model.LETTERS.toCharArray()) {
      JButton b = new JButton(String.valueOf(c));
      b.addActionListener(this);
      lowerPart.add(b);
    }

    add(upperPart);
    add(centerPart);
    add(lowerPart);
  }

  private void updateImageLabel(int count) {
    try {
      BufferedImage image = ImageIO.read(getClass().getResource("/hangman_" + count + ".png"));
      imageLabel.setIcon(new ImageIcon(image));
    } catch (IllegalArgumentException | IOException ex) {
      System.err.println("Can't load resource: " + ex.getMessage());
    }
  }


  public static void main(String[] args) {
    SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {
        new MainFrame().setVisible(true);
      }
    });

    System.out.println("Hello world!");
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (!model.isFinished()) {
      ((JButton) e.getSource()).setEnabled(false);
      char guess = e.getActionCommand().charAt(0);
      if (!model.updateGuessedWord(guess)) { // TODO
        updateImageLabel(model.incrementMissedCount());
        if (model.getMissCount() >= model.MAX_MISS) {
          model.setFinished(true);
          JOptionPane.showMessageDialog(this, "You loose!");
        }
      } else {
        guessedWordLabel.setText(model.getGuessedWord());
        if (!guessedWordLabel.getText().contains(".")) {
          model.setFinished(true);
          JOptionPane.showMessageDialog(this, "You win!");
        }
      }
    }
  }
}

package cz.cvut.fel.pjv;

public class Game {

    Player[] players;

    private final int end;

    public Game(Player[] players) {
        this.players = players;
        end = 50;
    }

    public boolean playRound() {
        for (Player player : players) {
            int playerPosition = player.advance();
            if (playerPosition == end) {
                return true;
            } else if (playerPosition > end) {
                int stepBack = 2 * (playerPosition - end);
                player.currentPosition -= stepBack;
            }
        }
        return false;
    }

    public void playGame() {
        boolean ret = false;
        while (!ret) {
            ret = playRound();
        }
        for (Player player : players) {
            if (player.getCurrentPosition() == end) {
                System.out.println("Winner: " + player.getName());
                break;
            }
        }
    }
}

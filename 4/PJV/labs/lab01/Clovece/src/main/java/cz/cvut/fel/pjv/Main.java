package cz.cvut.fel.pjv;

import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println("Choose number of players:");
        int n = sc.nextInt();
        System.out.println("Choose number of sides of the game cube");
        int sides = sc.nextInt();
        Dice dice = new Dice(sides);
        Player[] players = new Player[n];
        for (int i = 0; i < n; i++) {
            players[i] = new Player("Player " + (i + 1), dice);
        }
        Game game = new Game(players);
        game.playGame();
        sc.close();
    }
}

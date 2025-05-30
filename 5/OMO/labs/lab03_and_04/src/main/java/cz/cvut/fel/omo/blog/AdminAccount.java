package cz.cvut.fel.omo.blog;

import java.util.Scanner;

public class AdminAccount extends BlogAccount{

    private Scanner scanner = new Scanner(System.in);

    public AdminAccount(String username, String password, Blog blog){
        super(username, password, blog);
    }


}

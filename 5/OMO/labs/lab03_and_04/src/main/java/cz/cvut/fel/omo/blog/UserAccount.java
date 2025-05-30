package cz.cvut.fel.omo.blog;

public class UserAccount extends BlogAccount{

    boolean accountBlockage;

    public UserAccount(String username, String password, Blog blog){
        super(username, password, blog);
        accountBlockage = false;
    }

    public void blockAccount(){
        accountBlockage = true;
    }

    public void unblockAccount(){
        accountBlockage = false;
    }

    public boolean getAccountBlockage(){
        return accountBlockage;
    }

}

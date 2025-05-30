package cz.cvut.fel.omo.blog;

public abstract class BlogAccount {

    private String username;
    private String password;
    protected BlogInterface blog;

    public BlogAccount(String username, String password, Blog blog){
        this.username = username;
        this.password = password;
        this.blog = blog;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public boolean getAccountBlockage(){
        return false;
    }

    public String toString(){
        return username;
    }

    public void blockAccount(){}

}

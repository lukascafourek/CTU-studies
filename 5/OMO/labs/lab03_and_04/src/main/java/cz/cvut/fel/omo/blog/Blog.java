package cz.cvut.fel.omo.blog;

import java.util.ArrayList;
import java.util.List;


public class Blog implements BlogInterface {

    private Dashboard dashboard = new Dashboard();
    private List<BlogAccount> blogAccounts = new ArrayList<BlogAccount>();

    public void createNewAccount(String username, String password, boolean admin) {
        if (!existsUserAccount(username))
            if (admin)
                blogAccounts.add(new AdminAccount(username, password, this));
            else
                blogAccounts.add(new UserAccount(username, password, this));
    }

    public void blockUserAccount(String username){
        blogAccounts.stream()
                .filter(userAccount -> userAccount.getUsername().equals(username))
                .findFirst()
                .ifPresent(userAccount -> userAccount.blockAccount());
    }

    public boolean existsUserAccount(String username) {
        return blogAccounts.stream().anyMatch(userAccount -> userAccount.getUsername().equals(username));
    }

    public BlogAccount login(String username, String password) {
        return blogAccounts
                .stream()
                .filter(userAccount -> userAccount.getUsername().equals(username))
                .filter(userAccount -> userAccount.getPassword().equals(password))
                .filter(userAccount -> !userAccount.getAccountBlockage())
                .findFirst()
                .orElseGet(()->loginFailed());
    }

    public BlogAccount loginFailed(){
        // FIX ME
        return null;
    }

}

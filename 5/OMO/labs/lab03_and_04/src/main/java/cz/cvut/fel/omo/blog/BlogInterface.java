package cz.cvut.fel.omo.blog;


public interface BlogInterface {

    /**
     *   Factory method for creating user accounts.
     *   @param username  Account name serving as a unique identifier across user accounts.
     *   @param password  Password, that will be used to log into blog
     *   @param admin     Flag deciding, whether newly created object ought to be standard or admin account.
    */
    void createNewAccount(String username, String password, boolean admin);


    /**
     *  Method for blocking user account
     *  @param username  Username of account to be blocked
     */
    void blockUserAccount(String username);

    /**
    * This method goes through list of users and searches for given username.
    * @param username  Name of the blogAccount to be found
    * @return boolean  Does the blogAccount exists?
     */
    boolean existsUserAccount(String username);

    /**
    * This method simulates loginig into the blog. Method goes through list of user accounts searches for user
    * account.
    * @param username  Username of account to return.
    * @param password  Password for searched account.
    * @return UserAccount  UserAccount matching criteria or null.
    *
    */
    BlogAccount login(String username, String password);


}

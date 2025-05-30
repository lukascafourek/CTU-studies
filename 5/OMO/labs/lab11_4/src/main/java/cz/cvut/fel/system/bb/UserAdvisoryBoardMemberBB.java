/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import cz.cvut.fel.system.service.UserServiceImpl;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 *
 * @author Jirka
 */

@Component
@Scope("request")
public class UserAdvisoryBoardMemberBB {

//    @ManagedProperty(name = "userService", value = "#{userService}")
    @Autowired
    protected UserServiceImpl userService;
    protected String firstName;
    protected String lastName;
    protected String username;
    protected String password;
    //ROLE string

    public String saveUser() {
        getUserService().addAdvisoryBoardMember(firstName, lastName, username, password);

        return "success";

    }

    public UserServiceImpl getUserService() {
        return userService;
    }

    public void setUserService(UserServiceImpl userService) {
        this.userService = userService;
    }

    /**
     * getters and setters *
     */
    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
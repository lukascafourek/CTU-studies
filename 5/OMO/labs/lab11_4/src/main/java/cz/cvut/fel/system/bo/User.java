/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.UserDto;
import cz.cvut.fel.system.provider.HashProvider;
import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.Table;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;

/**
 *
 * @author Jirka
 */
@Configurable(preConstruction = true)
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@DiscriminatorColumn(name = "PERSON_ROLE")
@Table(name = "users")
public class User extends DomainEntity {

    @Column(length = 100, nullable = false, unique = true)
    String username;
    @Column(length = 40, nullable = false)
    String password;
    //Variables may be marked transient to indicate that they are not part of the persistent state of an object.
    @Autowired
    private transient HashProvider hashProvider; //transient fields are not persisted
    @Column(length = 40, nullable = false) //40 je hash od SHA1
    private String salt;
    @Enumerated(EnumType.STRING)
    private Role userRole;

    public String getSalt() {
        return salt;
    }

    public void setSalt(String salt) {
        this.salt = salt;
    }

    public HashProvider getHashProvider() {
        return hashProvider;
    }

    public void setHashProvider(HashProvider hashProvider) {
        this.hashProvider = hashProvider;
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
        //this.password = password;
        this.salt = hashProvider.computeHash(System.nanoTime() + "");
        this.password = hashProvider.computeHash(password + salt);
    }
    /*
     public Boolean getAdvisoryBoard() {
     return advisoryBoard;
     }

     public void setAdvisoryBoard(Boolean advisoryBoard) {
     this.advisoryBoard = advisoryBoard;
     }
     */

    public boolean hasPassword(String password) {
        if (hashProvider.computeHash(password + salt).equals(this.password)) {
            return true;
        }
        return false;
    }

    public Role getUserRole() {
        return userRole;
    }

    public void setUserRole(Role userRole) {
        this.userRole = userRole;
    }

//    public UserDto getDto() {
//        UserDto dto = new UserDto();
//        dto.setId(this.getId());
//        dto.setRole(this.getUserRole());
//        dto.setUsername(this.getUsername());
//        return dto;
//    }
}

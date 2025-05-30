/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 *
 * @author marcel
 */

@MappedSuperclass
public abstract class Person extends User {

    @Column(length = 100, nullable = false)
    String firstname;
    @Column(length = 100, nullable = false)
    String lastname;


    public String getFirstname() {
        return firstname;
    }

    public void setFirstname(String firstname) {
        this.firstname = firstname;
    }

    public String getLastname() {
        return lastname;
    }

    public void setLastname(String lastname) {
        this.lastname = lastname;
    }
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.AdminDto;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

/**
 *
 * @author Jirka
 */
@Entity
@DiscriminatorValue("AD")
public class Admin extends User {

    public Admin() {
        this.setUserRole(Role.ROLE_ADMIN);

    }

//    @Override
//    public AdminDto getDto() {
//        AdminDto dto = new AdminDto();
//        dto.setId(this.getId());
//                dto.setRole(this.getUserRole());
//
//        dto.setUsername(this.getUsername());
//        return dto;
//    }
}

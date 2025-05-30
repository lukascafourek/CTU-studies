/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.AdvisoryBoardMemberDto;
import javax.persistence.Entity;

/**
 *
 * @author Jirka
 */
@Entity
public class AdvisoryBoardMember extends Person {

    public AdvisoryBoardMember() {
        this.setUserRole(Role.ROLE_BOARD_MEMBER);

    }

//    @Override
//    public AdvisoryBoardMemberDto getDto() {
//        AdvisoryBoardMemberDto dto = new AdvisoryBoardMemberDto();
//        AdvisoryBoardMember a = this;
//        dto.setId(a.getId());
//        dto.setFirstname(a.getFirstname());
//        dto.setLastname(a.getLastname());
//        dto.setRole(a.getUserRole());
//        dto.setUsername(a.getUsername());
//        return dto;
//    }
}

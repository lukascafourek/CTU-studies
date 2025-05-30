/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.service;

import cz.cvut.fel.system.dto.AdminDto;
import cz.cvut.fel.system.dto.AdvisoryBoardMemberDto;
import cz.cvut.fel.system.dto.AuthorDto;
import cz.cvut.fel.system.dto.CorrectorDto;
import static org.junit.Assert.*;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 *
 * @author Jirka
 */
public class UserServiceImplTest extends AbstractServiceTest {

    @Autowired
    private UserServiceImpl userService;

    @Test
    public void testAddAndRetrieveAdmin() {

        String userName = "UserName1";
        String passwd = "passwd1";

        Long id = userService.addAdmin(userName, passwd);
        AdminDto userDto = userService.getAdminById(id);

        assertEquals(userName, userDto.getUsername());

    }

    @Test
    public void testAddAndRemoveAdmin() {

        String userName = "UserName2";
        String passwd = "passwd2";

        Long id = userService.addAdmin(userName, passwd);
        assertEquals(1, userService.getAllAdmins().size());
        userService.deleteAdmin(id);
        assertEquals(0, userService.getAllAdmins().size());
    }

    @Test
    public void testAddAndRetrieveAuthor() {

        String userName = "UserName3";
        String firstname = "firstname3";
        String lastname = "lastname3";
        String passwd = "passwd3";

        Long id = userService.addAuthor(firstname, lastname, userName, passwd);
        AuthorDto userDto = userService.getAuthorById(id);

        assertEquals(userName, userDto.getUsername());

    }

    @Test
    public void testAddAndRemoveAuthor() {

        String userName = "UserName4";
        String firstname = "firstname4";
        String lastname = "lastname4";
        String passwd = "passwd4";

        Long id = userService.addAuthor(firstname, lastname, userName, passwd);
        assertEquals(1, userService.getAllAuthors().size());
        userService.deleteAuthor(id);
        assertEquals(0, userService.getAllAuthors().size());
    }

    @Test
    public void testAddAndRetrieveCorrector() {

        String userName = "UserName5";
        String firstname = "firstname5";
        String lastname = "lastname5";
        String passwd = "passwd5";
        String email = "email5";

        Long id = userService.addCorrector(firstname, lastname, userName, passwd,email);
        CorrectorDto userDto = userService.getCorrectorById(id);

        assertEquals(userName, userDto.getUsername());

    }

    @Test
    public void testAddAndRemoveCorrector() {

        String userName = "UserName6";
        String firstname = "firstname6";
        String lastname = "lastname6";
        String passwd = "passwd6";
        String email = "email6";

        Long id = userService.addCorrector(firstname, lastname, userName, passwd,email);
        assertEquals(1, userService.getAllCorrectors().size());
        userService.deleteCorrector(id);
        assertEquals(0, userService.getAllCorrectors().size());
    }

    @Test
    public void testAddAndRetrieveBoardMember() {

        String userName = "UserName7";
        String firstname = "firstname7";
        String lastname = "lastname7";
        String passwd = "passwd7";

        Long id = userService.addAdvisoryBoardMember(firstname, lastname, userName, passwd);
        AdvisoryBoardMemberDto userDto = userService.getAdvisoryBoardMemberById(id);

        assertEquals(userName, userDto.getUsername());

    }

    @Test
    public void testAddAndRemoveBoardMember() {

        String userName = "UserName8";
        String firstname = "firstname8";
        String lastname = "lastname8";
        String passwd = "passwd8";

        Long id = userService.addAdvisoryBoardMember(firstname, lastname, userName, passwd);
        assertEquals(1, userService.getAllAdvisoryBoardMembers().size());
        userService.deleteAdvisoryBoardMember(id);
        assertEquals(0, userService.getAllAdvisoryBoardMembers().size());
    }
}

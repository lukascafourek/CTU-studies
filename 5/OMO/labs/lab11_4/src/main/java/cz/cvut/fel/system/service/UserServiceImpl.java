/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.service;

import cz.cvut.fel.system.bo.Admin;
import cz.cvut.fel.system.bo.Author;
import cz.cvut.fel.system.bo.Corrector;
import cz.cvut.fel.system.bo.AdvisoryBoardMember;
import cz.cvut.fel.system.bo.User;
import cz.cvut.fel.system.dto.AdminDto;
import cz.cvut.fel.system.dto.AuthorDto;
import cz.cvut.fel.system.dto.CorrectorDto;
import cz.cvut.fel.system.dto.AdvisoryBoardMemberDto;
import cz.cvut.fel.system.dto.DtoFactory;
import cz.cvut.fel.system.dto.UserDto;
import java.util.ArrayList;
import java.util.List;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author Jirka
 */
@Component
@Transactional
public class UserServiceImpl extends AbstractDataAccessService {
    //addCorrector, add
    //CRUD ke vsem rolim

    public UserDto getUserByUsername(String username) {
        User a = genericDao.getByPropertyUnique("username", username, User.class);
        UserDto dto = DtoFactory.getDto(a);
        return dto;
    }

    public UserDto getUserById(Long userId) {
        User a = genericDao.getById(userId, User.class);
        UserDto dto = DtoFactory.getDto(a);
        return dto;
    }
    //Author

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public Long addAuthor(String firstname, String lastname, String username, String password) throws IllegalArgumentException {
        List<User> userWithUsername = genericDao.getByProperty("username", username, User.class);
        if (!userWithUsername.isEmpty()) {
            throw new IllegalArgumentException("username: '" + username + "' is not available");
        }
        Author a = new Author();
        a.setFirstname(firstname);
        a.setLastname(lastname);
        a.setUsername(username);
        a.setPassword(password);
        return genericDao.saveOrUpdate(a).getId();
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public void deleteAuthor(Long userId) {
        genericDao.removeById(userId, Author.class);
    }

    public AuthorDto getAuthorById(Long id) {
        Author a = genericDao.getById(id, Author.class);
        AuthorDto dto = DtoFactory.getDto(a);
        return dto;
    }

    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_BOARD_MEMBER')")
    public List<AuthorDto> getAllAuthors() {
        List<Author> users = genericDao.getAll(Author.class);
        List<AuthorDto> userDtos = new ArrayList<AuthorDto>();
        for (Author a : users) {
            AuthorDto dto = DtoFactory.getDto(a);
            userDtos.add(dto);
        }
        return userDtos;
    }

    //Corrector
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public Long addCorrector(String firstname, String lastname, String username, String password, String email) {
        Corrector a = new Corrector();
        a.setFirstname(firstname);
        a.setLastname(lastname);
        a.setUsername(username);
        a.setPassword(password);
        a.setEmail(email);
        return genericDao.saveOrUpdate(a).getId();
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public void deleteCorrector(Long userId) {
        genericDao.removeById(userId, Corrector.class);
    }

    public CorrectorDto getCorrectorById(Long id) {
        Corrector a = genericDao.getById(id, Corrector.class);
        CorrectorDto dto = DtoFactory.getDto(a);
        return dto;
    }

    @PreAuthorize("hasRole('ROLE_ADMIN') or hasRole('ROLE_BOARD_MEMBER')")
    public List<CorrectorDto> getAllCorrectors() {
        List<Corrector> users = genericDao.getAll(Corrector.class);
        List<CorrectorDto> userDtos = new ArrayList<CorrectorDto>();

        for (Corrector a : users) {
            CorrectorDto dto = DtoFactory.getDto(a);
            userDtos.add(dto);
        }
        return userDtos;
    }

    //AdvisoryBoardMember
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public Long addAdvisoryBoardMember(String firstname, String lastname, String username, String password) {
        AdvisoryBoardMember a = new AdvisoryBoardMember();
        a.setFirstname(firstname);
        a.setLastname(lastname);
        a.setUsername(username);
        a.setPassword(password);
        return genericDao.saveOrUpdate(a).getId();
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public void deleteAdvisoryBoardMember(Long userId) {
        genericDao.removeById(userId, AdvisoryBoardMember.class);
    }

    public AdvisoryBoardMemberDto getAdvisoryBoardMemberById(Long id) {
        AdvisoryBoardMember a = genericDao.getById(id, AdvisoryBoardMember.class);
        AdvisoryBoardMemberDto dto = DtoFactory.getDto(a);
        return dto;
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public List<AdvisoryBoardMemberDto> getAllAdvisoryBoardMembers() {
        List<AdvisoryBoardMember> users = genericDao.getAll(AdvisoryBoardMember.class);
        List<AdvisoryBoardMemberDto> userDtos = new ArrayList<AdvisoryBoardMemberDto>();

        for (AdvisoryBoardMember a : users) {
            AdvisoryBoardMemberDto dto = DtoFactory.getDto(a);
            userDtos.add(dto);
        }
        return userDtos;
    }

    //admin
    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public Long addAdmin(String username, String password) {
        Admin a = new Admin();
        a.setUsername(username);
        a.setPassword(password);
        return genericDao.saveOrUpdate(a).getId();
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public void deleteAdmin(Long userId) {
        genericDao.removeById(userId, Admin.class);
    }

    public AdminDto getAdminById(Long id) {
        Admin a = genericDao.getById(id, Admin.class);
        AdminDto dto = DtoFactory.getDto(a);
        return dto;
    }

    @PreAuthorize("hasRole('ROLE_ADMIN')")
    public List<AdminDto> getAllAdmins() {
        List<Admin> users = genericDao.getAll(Admin.class);
        List<AdminDto> userDtos = new ArrayList<AdminDto>();

        for (Admin a : users) {
            AdminDto dto = DtoFactory.getDto(a);
            userDtos.add(dto);
        }
        return userDtos;
    }
}

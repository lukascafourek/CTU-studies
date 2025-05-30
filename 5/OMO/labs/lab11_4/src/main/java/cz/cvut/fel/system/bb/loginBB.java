/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import cz.cvut.fel.system.bo.Role;
import cz.cvut.fel.system.dto.UserDto;
import cz.cvut.fel.system.service.UserServiceImpl;
import java.util.Locale;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

/**
 *
 * @author Jirka
 */
@Component
@Scope("session")
//@ApplicationScoped
public class loginBB {

    String username;
    String role;
    String lang;
    long id;
    @Autowired
    protected UserServiceImpl userService;

    public String getUsername() {
        Object user = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (user instanceof UserDetails) {
            this.username = ((UserDetails) user).getUsername();
            //fing user id by username
        }
        return username;
    }

    public String getRole() {
        if (role == null) {

            UserDto user = userService.getUserByUsername(this.getUsername());
            Role roletemp = user.getRole();
            this.role = roletemp.name();
        }
        return role;
    }

    public void setRole(String role) {
        UserDto user = userService.getUserByUsername(this.getUsername());
        Role roletemp = user.getRole();
        String strRole = roletemp.name();
        this.role = strRole;
    }

    public long getId() {
        UserDto user = userService.getUserByUsername(this.getUsername());
        this.id = user.getId();
        return this.id;
    }

    public void setId(long id) {
        UserDto user = userService.getUserByUsername(this.getUsername());
        long idtemp = user.getId();
        this.id = idtemp;
    }

    public String getLang() {
        return lang;
    }

    public void setLang(String lang) {
        this.lang = lang;
    }

    //change of language
    public void changeLang(String langCode) {
        //FacesContext.getCurrentInstance().getViewRoot().setLocale(new Locale(langCode));
        this.lang = langCode;
        if (langCode.equals("en")) {
            FacesContext.getCurrentInstance().getViewRoot().setLocale(new Locale("en"));
        } else {
            FacesContext.getCurrentInstance().getViewRoot().setLocale(new Locale("cs"));
        }

        //return null;
    }
}

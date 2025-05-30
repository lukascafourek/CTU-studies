/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import cz.cvut.fel.system.service.UserServiceImpl;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 *
 * @author marcel
 */
@Component
@Scope("request")
public class HelperBB {
    List<String> allCorrectors;

    @Autowired
    protected UserServiceImpl userServiceImpl;
    
    
}

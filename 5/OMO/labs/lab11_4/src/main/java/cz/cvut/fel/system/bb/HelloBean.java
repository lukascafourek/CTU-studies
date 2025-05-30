/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import java.util.Date;
import javax.faces.bean.ManagedBean;

/**
 *
 * @author Jirka
 */
@ManagedBean//trida ale JSF anotace... aplikace proleze vsechny managed beany a vytvori objekty

public class HelloBean {
    
    protected String message = "beana test! Výpis z beany";
    protected String name;
    
    public String getMessage(){
        return message;
    }
    
    public void setMessage(String message){
        this.message = message;
    }
    public String getName(){
        return name;
    }
    public void setName(String name){
        this.name = name;
    }
    
    
        public String sayHello(){
            return "greetings.xtml";        
        }
    
    public String getDate(){
        return new Date().toString();
    }
    
    
}

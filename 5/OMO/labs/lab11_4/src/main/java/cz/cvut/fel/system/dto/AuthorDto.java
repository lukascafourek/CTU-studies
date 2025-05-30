/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.dto;

import java.util.List;

/**
 *
 * @author Jirka
 */
public class AuthorDto extends PersonAbstractDto {

    List<Long> articleIds;

    public AuthorDto() {
    }



    public List<Long> getArticleIds() {
        return articleIds;
    }

    public void setArticleIds(List<Long> articleIds) {
        this.articleIds = articleIds;
    }
    
    
    
    
}

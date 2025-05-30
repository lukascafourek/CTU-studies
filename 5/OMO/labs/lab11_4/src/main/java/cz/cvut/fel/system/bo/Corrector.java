/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.CorrectorDto;
import cz.cvut.fel.system.helper.DtoTransformerHelper;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.OneToMany;

/**
 *
 * @author Jirka
 */
@Entity
@DiscriminatorValue("CO")
public class Corrector extends Person {

    String email;
    @OneToMany(mappedBy = "corrector", cascade = CascadeType.ALL)
    private List<Article> articles;

    public Corrector() {
        this.setUserRole(Role.ROLE_CORRECTOR);
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    //article
    public List<Article> getArticles() {
        return articles;
    }

    public void setArticles(List<Article> articles) {
        this.articles = articles;
    }

    public void addArticle(Article article) {
        if (this.articles == null) {
            articles = new ArrayList<Article>();
        }
        if (!this.articles.contains(article)) {
            articles.add(article);
        }
    }

//    @Override
//    public CorrectorDto getDto() {
//        Corrector a = this;
//        CorrectorDto dto = new CorrectorDto();
//        dto.setId(a.getId());
//        dto.setRole(a.getUserRole());
//        dto.setFirstname(a.getFirstname());
//        dto.setLastname(a.getLastname());
//        dto.setUsername(a.getUsername());
//        dto.setArticleIds(DtoTransformerHelper.getIdentifiers(a.getArticles()));
//        dto.setEmail(a.getEmail());
//
//        return dto;
//    }
}

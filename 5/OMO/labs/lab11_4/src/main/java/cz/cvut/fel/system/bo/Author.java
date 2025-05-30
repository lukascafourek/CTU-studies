/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.AuthorDto;
import cz.cvut.fel.system.helper.DtoTransformerHelper;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.ManyToMany;

/**
 *
 * @author Jirka
 */
@Entity
@DiscriminatorValue("AU")
public class Author extends Person {

    @ManyToMany(cascade = CascadeType.ALL)
    private List<Article> articles;

    public Author() {
                this.setUserRole(Role.ROLE_AUTHOR);

    }

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

    public void removeArticle(Article article) {
        if (this.articles == null) {
            return;
        }
        this.articles.remove(article);
    }



//    @Override
//    public AuthorDto getDto() {
//        AuthorDto dto = new AuthorDto();
//        Author a = this;
//        dto.setFirstname(a.getFirstname());
//        dto.setLastname(a.getLastname());
//        dto.setId(a.getId());
//        dto.setRole(a.getUserRole());
//        dto.setUsername(a.getUsername());
//        dto.setArticleIds(DtoTransformerHelper.getIdentifiers(a.getArticles()));
//        return dto;
//    }
}

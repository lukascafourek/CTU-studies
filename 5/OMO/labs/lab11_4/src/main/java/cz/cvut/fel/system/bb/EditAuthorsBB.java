/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.dto.AuthorDto;
import cz.cvut.fel.system.dto.CorrectorDto;
import cz.cvut.fel.system.dto.VersionDto;
import cz.cvut.fel.system.service.ArticleServiceImpl;
import cz.cvut.fel.system.service.UserServiceImpl;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.faces.context.FacesContext;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.DataModel;
import javax.faces.model.ListDataModel;
import javax.faces.model.SelectItem;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 *
 * @author marcel
 */
@Component
@Scope("view")
public class EditAuthorsBB {

    String articleId;
    private DataModel<AuthorDto> authorDtos;
    private Long articleIdLong;
    private String name;
    @Autowired
    private ArticleServiceImpl articleService;
    @Autowired
    UserServiceImpl userServiceImpl;
    String selectedAuthorId;

    public String getSelectedAuthorId() {
        return selectedAuthorId;
    }

    public void setSelectedAuthorId(String selectedAuthorId) {
        this.selectedAuthorId = selectedAuthorId;
    }

    @PostConstruct
    public void init() {
        String readArticleId = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("articleId");

        if (readArticleId != null && !"".equals(readArticleId)) {
            articleId = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("articleId");
        }

        if (articleId == null) {
            return;
        }
        articleIdLong = Long.parseLong(articleId.trim());
        ArticleDto adto = articleService.getArticleById(articleIdLong);
        this.name = adto.getName();


        List<AuthorDto> authors = new ArrayList<AuthorDto>();
        for (Long authorId : adto.getAuthorsIDs()) {
            AuthorDto authordto = userServiceImpl.getAuthorById(authorId);
            authors.add(authordto);
        }
        authorDtos = new ListDataModel<AuthorDto>(authors);
    }

    public String deleteAuthor() {
        AuthorDto selectedAuthor = authorDtos.getRowData();
        articleService.removeAuthorFromArticle(articleIdLong, selectedAuthor.getId());
        init();
        return "/editAuthors";
        //FacesUtil.addMessage("User was sucessfully deleted");
    }

    public String getArticleId() {
        return articleId;
    }

    public void setArticleId(String articleId) {
        this.articleId = articleId;
    }

    public DataModel<AuthorDto> getAuthorDtos() {
        return authorDtos;
    }

    public void setAuthorDtos(DataModel<AuthorDto> authorDtos) {
        this.authorDtos = authorDtos;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ArticleServiceImpl getArticleService() {
        return articleService;
    }

    public void setArticleService(ArticleServiceImpl articleService) {
        this.articleService = articleService;
    }

    public SelectItem[] getAllAuthors() {
        List<AuthorDto> allAuthors = userServiceImpl.getAllAuthors();
        for (AuthorDto a : authorDtos) {
            allAuthors.remove(a);
        }
        SelectItem[] items = new SelectItem[allAuthors.size()];

        int i = 0;
        for (AuthorDto g : allAuthors) {
            items[i++] = new SelectItem(g.getId(), g.getFirstname() + " " + g.getLastname());
        }
        return items;
    }

    public void selectedAuthorChange(ValueChangeEvent e) {
//        System.out.println("****fff****");
//        System.out.println(e.getSource());
//        System.out.println(e.getNewValue());
    }

    public String submit() {
        if (!selectedAuthorId.equals("none")) {
            AuthorDto ad = userServiceImpl.getAuthorById(Long.parseLong(selectedAuthorId));
            articleService.addAuthorToArticle(articleIdLong, ad.getId());
        }
        init();

        return "/editAuthors";
    }
}

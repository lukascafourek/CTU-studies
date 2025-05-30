/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import static cz.cvut.fel.system.bb.ArticleBB.LOCATION;
import cz.cvut.fel.system.bo.Role;
import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.dto.UserDto;
import cz.cvut.fel.system.service.ArticleServiceImpl;
import cz.cvut.fel.system.service.UserServiceImpl;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import javax.annotation.PostConstruct;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.ServletContext;
import org.apache.commons.io.FilenameUtils;
import org.primefaces.model.UploadedFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

/**
 *
 * @author marcel
 */
@Component
@Scope("request")
public class AddArticleAnnotationBB {

//@ManagedProperty(name="articleService", value="#{articleService}" )
    @Autowired
    protected ArticleServiceImpl articleService;
    @Autowired
    protected UserServiceImpl userServiceImpl;
    @Value("#{request.getParameter('articleId')}")
    private String articleId;
    protected String name;
    protected Long articleIdLong;
    protected UploadedFile file;
    protected String annotationText;
    @Autowired
    ArticleDetailBB articleDetailBB;

    public String getAnnotationText() {
        return annotationText;
    }

    public void setAnnotationText(String annotationText) {
        this.annotationText = annotationText;
    }

    @PostConstruct
    public void init() {
        if (articleId == null) {
            return;
        }

        articleIdLong = Long.parseLong(articleId.trim());
        ArticleDto adto = articleService.getArticleById(articleIdLong);
        this.name = adto.getName();

    }

    public String addAnnotation(String articleId) throws Exception {
        this.articleId = articleId;
        init();
        if (articleIdLong == null) {
            throw new Exception("missing article id");
        }
        Long userId = null;
        Object user = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (user instanceof UserDetails) {
            String username = ((UserDetails) user).getUsername();
            UserDto udto = userServiceImpl.getUserByUsername(username);
            userId = udto.getId();
        }
        articleService.addAnnotationToArticle(articleIdLong, userId, annotationText, null);

        articleDetailBB.setMessage("***Article version added***");
        articleDetailBB.setArticleId(articleId);
        articleDetailBB.init();
        return "/detailArticle";
    }

    public ArticleServiceImpl getArticleService() {
        return articleService;
    }

    public void setArticleService(ArticleServiceImpl articleService) {
        this.articleService = articleService;
    }

    public String getArticleId() {
        return articleId;
    }

    public void setArticleId(String articleId) {
        this.articleId = articleId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getArticleIdLong() {
        return articleIdLong;
    }

    public void setArticleIdLong(Long articleIdLong) {
        this.articleIdLong = articleIdLong;
    }

    public UploadedFile getFile() {
        return file;
    }

    public void setFile(UploadedFile file) {
        this.file = file;
    }
}

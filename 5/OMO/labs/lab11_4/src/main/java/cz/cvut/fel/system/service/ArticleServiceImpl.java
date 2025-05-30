/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.service;

import cz.cvut.fel.system.bo.Annotation;
import cz.cvut.fel.system.bo.Article;
import cz.cvut.fel.system.bo.Author;
import cz.cvut.fel.system.bo.Corrector;
import cz.cvut.fel.system.bo.Status;
import cz.cvut.fel.system.bo.User;
import cz.cvut.fel.system.bo.Version;
import cz.cvut.fel.system.dto.AnnotationDto;
import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.dto.DtoFactory;
import cz.cvut.fel.system.dto.VersionDto;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author marcel
 */
@Component
@Transactional
public class ArticleServiceImpl extends AbstractDataAccessService {

    @Transactional
    @PreAuthorize("hasRole('ROLE_AUTHOR')")
    public Long addArticle(String pathOfFile, String name, Boolean priority, List<Long> authorIDs) {
        Article article = new Article();
        article.setName(name);
        article.setStatus(Status.entrance_notGranted);
        Version firstVersion = new Version();
        firstVersion.setDatetime(new Date());
        firstVersion.setArticle(article);
        firstVersion.setPathOfFile(pathOfFile);
        genericDao.saveOrUpdate(firstVersion);
        article.addVersion(firstVersion);
        if (priority == null) {
            priority = false;
        }
        article.setPriority(priority);
        genericDao.saveOrUpdate(article);
        if (authorIDs != null) {
            for (Long authorID : authorIDs) {
                Author author = genericDao.getById(authorID, Author.class);

                author.addArticle(article);
                genericDao.saveOrUpdate(author);
                article.addAuthor(author);
            }
        }
        genericDao.saveOrUpdate(article);
        return article.getId();
    }

    @Transactional
    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR')")
    public void addAuthorToArticle(Long articleID, Long authorID) {
        Article article = genericDao.getById(articleID, Article.class);
        Author author = genericDao.getById(authorID, Author.class);
        article.addAuthor(author);
        genericDao.saveOrUpdate(article);
        author.addArticle(article);
        genericDao.saveOrUpdate(author);
    }

    @Transactional
    @PreAuthorize("hasRole('ROLE_AUTHOR') or hasRole('ROLE_BOARD_MEMBER')")
    public void removeAuthorFromArticle(Long articleID, Long authorID) {
        Article article = genericDao.getById(articleID, Article.class);
        Author author = genericDao.getById(authorID, Author.class);
        article.removeAuthor(author);
        genericDao.saveOrUpdate(article);
        author.removeArticle(article);
        genericDao.saveOrUpdate(author);
    }

    @Transactional
    @PreAuthorize("hasRole('ROLE_AUTHOR')")
    public void addVersionOfArticle(Long articleID, String pathOfFile) {
        Article a = genericDao.getById(articleID, Article.class);
        Version v = new Version();
        v.setArticle(a);
        v.setDatetime(new Date());
        v.setPathOfFile(pathOfFile);
        genericDao.saveOrUpdate(v);
        a.addVersion(v);
        genericDao.saveOrUpdate(a);

    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public Long addAnnotationToArticle(Long articleID, Long authorOfAnnotationID, String annotationText, String PathOfFile) {
        Article a = genericDao.getById(articleID, Article.class);
        User u = genericDao.getById(authorOfAnnotationID, User.class);
        Annotation an = new Annotation();
        an.setArticle(a);
        a.addAnnotation(an);
        an.setAuthorOfAnnotation(u);
        an.setAnnotationText(annotationText);
        an.setPathOfFile(PathOfFile);
        an = genericDao.saveOrUpdate(an);
        return an.getId();
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR')")
    public void deleteArticle(long articleId) {
        genericDao.removeById(articleId, Article.class);
    }

    @PreAuthorize("hasRole('ROLE_AUTHOR') or hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR')")
    public void deleteArticleVersion(long versionId) {
        genericDao.removeById(versionId, Version.class);
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public ArticleDto getArticleById(Long id) {
        Article a = genericDao.getById(id, Article.class);
        ArticleDto dto = DtoFactory.getDto(a);
        return dto;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public VersionDto getNewestVersionOfArticle(Long articleID) {
        Article a = genericDao.getById(articleID, Article.class);
        if (!a.getVersions().isEmpty()) {
            Version newest = a.getVersions().get(0);
            VersionDto dto = DtoFactory.getDto(newest);
            return dto;
        } else {
            return null;
        }

    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public List<VersionDto> getAllVersionsOfArticle(Long articleID) {
        Article a = genericDao.getById(articleID, Article.class);
        List<VersionDto> versionDtos = new ArrayList<VersionDto>();
        if (a != null && a.getVersions() != null) {
            for (Version v : a.getVersions()) {
                VersionDto dto = DtoFactory.getDto(v);
                versionDtos.add(dto);
            }
        }
        return versionDtos;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public List<ArticleDto> getAllArticles() {
        List<Article> articles = genericDao.getAll(Article.class);
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();
        for (Article a : articles) {
            ArticleDto dto = DtoFactory.getDto(a);
            articleDtos.add(dto);
        }
        Collections.sort(articleDtos, new ComparArticle());
        return articleDtos;
    }

    @PreAuthorize("hasRole('ROLE_CORRECTOR') or hasRole('ROLE_BOARD_MEMBER')")
    public List<ArticleDto> getArticlesForCorrector(long correctorID) {
        List<Article> articles = genericDao.getByProperty("corrector.id", correctorID, Article.class);
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();

        for (Article a : articles) {
            ArticleDto dto = DtoFactory.getDto(a);
            articleDtos.add(dto);
        }
        return articleDtos;
    }

    @PreAuthorize("hasRole('ROLE_AUTHOR') or hasRole('ROLE_BOARD_MEMBER')")
    public List<ArticleDto> getArticlesForAuthor(long authorID) {
        Author author = genericDao.getById(authorID, Author.class);
        List<Article> articles = author.getArticles();
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();

        for (Article a : articles) {
            ArticleDto dto = DtoFactory.getDto(a);
            articleDtos.add(dto);
        }
        return articleDtos;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR') or hasRole('ROLE_CORRECTOR')")
    public List<AnnotationDto> getAnnotationsForArticle(long articleID) {
        List<Annotation> annotations = genericDao.getByProperty("article.id", articleID, Annotation.class);
        List<AnnotationDto> annotationDtos = new ArrayList<AnnotationDto>();

        for (Annotation a : annotations) {
            annotationDtos.add(DtoFactory.getDto(a));
        }
        return annotationDtos;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER')")
    public List<ArticleDto> getArticlesByState(Status status) {
        List<Article> articles = genericDao.getByProperty("status", status, Article.class);
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();

        for (Article a : articles) {
            ArticleDto dto = DtoFactory.getDto(a);
            articleDtos.add(dto);
        }
        return articleDtos;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER')")
    public List<ArticleDto> getPriorityArticles() {
        List<Article> articles = genericDao.getAllPriorityArticles();
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();
        for (Article a : articles) {
            articleDtos.add(DtoFactory.getDto(a));
        }
        return articleDtos;
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER')")
    public List<ArticleDto> getArticlesByPriority(Boolean priority) {
        List<Article> articles = genericDao.getByProperty("priority", priority, Article.class);
        List<ArticleDto> articleDtos = new ArrayList<ArticleDto>();
        for (Article a : articles) {
            articleDtos.add(DtoFactory.getDto(a));
        }
        return articleDtos;
    }

    @Transactional
    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER')")
    public void setCorrectorOfArticle(long articleID, long correctorID) {
        Article article = genericDao.getById(articleID, Article.class);
        Corrector c = genericDao.getById(correctorID, Corrector.class);
        article.setCorrector(c);
        article.setStatus(Status.entrance_granted);
        c.addArticle(article);
        genericDao.saveOrUpdate(c);
        genericDao.saveOrUpdate(article);
    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR')")
    public void setStatusOfArticle(Status status, long articleID) {
        Article article = genericDao.getById(articleID, Article.class);
        article.setStatus(status);
        genericDao.saveOrUpdate(article);

    }

    @PreAuthorize("hasRole('ROLE_BOARD_MEMBER') or hasRole('ROLE_AUTHOR')")
    public void setPriorityOfArticle(long articleID, Boolean priority) {
        Article article = genericDao.getById(articleID, Article.class);
        article.setPriority(priority);
        genericDao.saveOrUpdate(article);

    }

    @PreAuthorize("hasRole('ROLE_CORRECTOR')")
    public void markArticleAsCorrected(Long articleID) {
        Article a = genericDao.getById(articleID, Article.class);
        a.setStatus(Status.checked);
        genericDao.saveOrUpdate(a);

    }
}

class ComparArticle implements Comparator<ArticleDto> {

    @Override
    public int compare(ArticleDto o1, ArticleDto o2) {
        return o1.getId().compareTo(o2.getId());
    }
}

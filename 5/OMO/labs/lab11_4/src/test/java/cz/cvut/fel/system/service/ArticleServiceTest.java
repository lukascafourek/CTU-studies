/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.service;

import cz.cvut.fel.system.bo.Status;
import cz.cvut.fel.system.dto.AnnotationDto;
import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.dto.VersionDto;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;
import org.springframework.beans.factory.annotation.Autowired;

/**
 *
 * @author marcel
 */
public class ArticleServiceTest extends AbstractServiceTest {

    @Autowired
    private ArticleServiceImpl articleService;
    @Autowired
    private UserServiceImpl userService;
    private String pathOfFile = "pathOfFile", pathOfFile2 = "pathOfFile2", pathOfFile3 = "pathOfFile3", name = "name";
    private Boolean priority = true;
    String userName1 = "UserName1";
    String userName2 = "UserName2";
    String userName3 = "UserName3";
    String userName4 = "UserName4";
    String userName5 = "UserName5";
    String firstname = "firstname";
    String lastname = "lastname";
    String passwd = "passwd";

    public ArticleServiceTest() {
    }

    /**
     * Test of addArticle method, of class ArticleService.
     */
    @Test
    public void testAddAndRetrieveArticleAddAndRemoveAuthors() {
        Long idA1 = userService.addAuthor(firstname, lastname, userName1, passwd);
        Long idA2 = userService.addAuthor(firstname, lastname, userName2, passwd);
        Long idA3 = userService.addAuthor(firstname, lastname, userName3, passwd);
        Long idA4 = userService.addAuthor(firstname, lastname, userName4, passwd);
        Long idA5 = userService.addAuthor(firstname, lastname, userName5, passwd);

        assertEquals(5, userService.getAllAuthors().size());


        List<Long> authors = new ArrayList<Long>();
        authors.add(idA1);
        authors.add(idA2);
        authors.add(idA3);
        Long articleID = articleService.addArticle(pathOfFile, name, priority, authors);
        ArticleDto articleDto = articleService.getArticleById(articleID);
        //assertEquals(name, articleDto.getName());

        assertEquals(3, articleDto.getAuthorsIDs().size());


        List<ArticleDto> articlesOfAuthor = articleService.getArticlesForAuthor(idA1);

        assertEquals(articleID, articlesOfAuthor.get(0).getId());

        List<VersionDto> versions = articleService.getAllVersionsOfArticle(articleID);
        assertEquals(1, versions.size());

        articleService.addAuthorToArticle(articleID, idA4);
        articleService.addAuthorToArticle(articleID, idA5);
        articleDto = articleService.getArticleById(articleID);

        assertEquals(5, articleDto.getAuthorsIDs().size());

        articleService.removeAuthorFromArticle(articleID, idA1);
        articleDto = articleService.getArticleById(articleID);

        assertEquals(4, articleDto.getAuthorsIDs().size());


    }

    @Test
    public void testArticleVersions() {
        Long articleID = articleService.addArticle(pathOfFile, name, priority, null);
        List<VersionDto> versions = articleService.getAllVersionsOfArticle(articleID);
        System.out.println(versions);
        assertEquals(1, versions.size());
        assertEquals(articleID, versions.get(0).getArticleID());

        articleService.addVersionOfArticle(articleID, pathOfFile2);

        articleService.addVersionOfArticle(articleID, pathOfFile3);
        versions = articleService.getAllVersionsOfArticle(articleID);
        assertEquals(3, versions.size());

        VersionDto newest = articleService.getNewestVersionOfArticle(articleID);
        System.out.println(newest);

        assertEquals(pathOfFile3, newest.getPathOfFile());




    }

    @Test
    public void testAddAndRemoveArticle() {
        Long articleID = articleService.addArticle(pathOfFile, name, priority, null);
        List<ArticleDto> articles = articleService.getAllArticles();
        assertEquals(1, articles.size());
        articleService.deleteArticle(articleID);
        articles = articleService.getAllArticles();
        assertEquals(0, articles.size());
    }

    @Test
    public void testAddAndGetAnnotations() {
        Long articleID = articleService.addArticle(pathOfFile, name, priority, null);
        Long idA1 = userService.addAuthor(firstname, lastname, userName1, passwd);
        articleService.addAnnotationToArticle(articleID, idA1, "text", pathOfFile);
        articleService.addAnnotationToArticle(articleID, idA1, "text", pathOfFile);
        List<AnnotationDto> annotations = articleService.getAnnotationsForArticle(articleID);
        assertEquals(2, annotations.size());
    }

    @Test
    public void testSetCorrectorAndRetrieve() {
        Long articleID = articleService.addArticle(pathOfFile, name, priority, null);
        Long article2ID = articleService.addArticle(pathOfFile2, name, priority, null);
        Long correctorID = userService.addCorrector(firstname, lastname, userName1, passwd, name);
        articleService.setCorrectorOfArticle(article2ID, correctorID);
        List<ArticleDto> articles = articleService.getArticlesForCorrector(correctorID);
        assertEquals(name, articles.get(0).getName());
        articleService.setCorrectorOfArticle(articleID, correctorID);
        articles = articleService.getArticlesForCorrector(correctorID);
        assertEquals(2, articles.size());


    }

    @Test
    public void testArticleStatusChanging() {
        Long articleID = articleService.addArticle(pathOfFile, name, priority, null);
        Long article2ID = articleService.addArticle(pathOfFile2, name, priority, null);
        Long correctorID = userService.addCorrector(firstname, lastname, userName1, passwd, name);
        List<ArticleDto> notGranted = articleService.getArticlesByState(Status.entrance_notGranted);
        assertEquals(2, notGranted.size());
        articleService.setCorrectorOfArticle(articleID, correctorID);
        notGranted = articleService.getArticlesByState(Status.entrance_notGranted);
        assertEquals(1, notGranted.size());
        List<ArticleDto> granted = articleService.getArticlesByState(Status.entrance_granted);
        assertEquals(1, granted.size());
        articleService.markArticleAsCorrected(articleID);
        List<ArticleDto> corrected = articleService.getArticlesByState(Status.checked);
        assertEquals(1, corrected.size());
        articleService.setStatusOfArticle(Status.checked, article2ID);
        corrected = articleService.getArticlesByState(Status.checked);
        assertEquals(2, corrected.size());

    }

    @Test
    public void testPriorityArticles() {
        Long articleID = articleService.addArticle(pathOfFile, name, null, null);
        Long article2ID = articleService.addArticle(pathOfFile2, name, null, null);
        Long article3ID = articleService.addArticle(pathOfFile2, name, null, null);
        Long article4ID = articleService.addArticle(pathOfFile2, name, null, null);
        articleService.setPriorityOfArticle(articleID, true);
        articleService.setPriorityOfArticle(article2ID, false);
        List<ArticleDto> priorities = articleService.getArticlesByPriority(true);
        List<ArticleDto> notpriorities = articleService.getArticlesByPriority(false);
        List<ArticleDto> nullpriorities = articleService.getArticlesByPriority(null);
        assertEquals(1, priorities.size());
        assertEquals(3, notpriorities.size());
        assertEquals(0, nullpriorities.size());
        priorities = articleService.getPriorityArticles();
        assertEquals(1, priorities.size());



    }
}
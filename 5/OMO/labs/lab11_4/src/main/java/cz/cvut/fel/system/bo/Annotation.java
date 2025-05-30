/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.AnnotationDto;
import cz.cvut.fel.system.helper.HibernateTools;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

/**
 *
 * @author Jirka
 */
@Entity
public class Annotation extends DomainEntity {

    @ManyToOne
    User authorOfAnnotation;
    @Column(length = 100, nullable = true)
    String pathOfFile;
    @Column(length = 100, nullable = true)
    String annotationText;
    @ManyToOne
    Article article;

    public Annotation() {
    }

    public String getPathOfFile() {
        return pathOfFile;
    }

    public void setPathOfFile(String pathOfFile) {
        this.pathOfFile = pathOfFile;
    }

    public String getAnnotationText() {
        return annotationText;
    }

    public void setAnnotationText(String annotationText) {
        this.annotationText = annotationText;
    }

    public Article getArticle() {
        return article;
    }

    public void setArticle(Article article) {
        this.article = article;
        article.addAnnotation(this);
    }

    public User getAuthorOfAnnotation() {
        return authorOfAnnotation;
    }

    public void setAuthorOfAnnotation(User authorOfAnnotation) {
        this.authorOfAnnotation = authorOfAnnotation;
    }

//    public AnnotationDto getDto() {
//        AnnotationDto dto = new AnnotationDto();
//        dto.setAnnotation(this.getAnnotationText());
//        dto.setArticleId(HibernateTools.getIdentifier(this.getArticle()));
//        dto.setAuthorOfAnnotationId(HibernateTools.getIdentifier(this.getAuthorOfAnnotation()));
//        dto.setId(this.getId());
//        dto.setPathOfFile(this.getPathOfFile());
//        return dto;
//    }
}

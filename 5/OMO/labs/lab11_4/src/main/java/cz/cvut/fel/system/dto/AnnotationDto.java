/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.dto;

/**
 *
 *
 * @author Jirka
 */
public class AnnotationDto extends AbstractDto {

    String pathOfFile;
    String annotation;
    Long articleId;
    Long authorOfAnnotationId;

    public AnnotationDto() {
    }

    public AnnotationDto(Long id, String pathOfFile, String annotation, Long articleId, Long authorOfAnnotationId) {
        this.id = id;
        this.pathOfFile = pathOfFile;
        this.annotation = annotation;
        this.articleId = articleId;
        this.authorOfAnnotationId = authorOfAnnotationId;
    }

    public String getPathOfFilel() {
        return pathOfFile;
    }

    public void setPathOfFile(String pathOfFile) {
        this.pathOfFile = pathOfFile;
    }

    public String getAnnotation() {
        return annotation;
    }

    public void setAnnotation(String annotation) {
        this.annotation = annotation;
    }

    public Long getArticleId() {
        return articleId;
    }

    public void setArticleId(Long articleId) {
        this.articleId = articleId;
    }

    public Long getAuthorOfAnnotationId() {
        return authorOfAnnotationId;
    }

    public void setAuthorOfAnnotationId(Long authorOfAnnotationId) {
        this.authorOfAnnotationId = authorOfAnnotationId;
    }
}

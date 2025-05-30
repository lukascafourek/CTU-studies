/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.helper.DtoTransformerHelper;
import cz.cvut.fel.system.helper.HibernateTools;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;

/**
 *
 * @author Jirka
 */
@Entity
@DiscriminatorValue("AR")
@NamedQuery(
        name = "Article.getPriorityArticles",
        query = "SELECT a FROM Article a WHERE a.priority=true")
public class Article extends DomainEntity {

    @Column(length = 128)
    String name;
    @Column()
    Boolean priority;
    @Enumerated(EnumType.STRING)
    Status status;
    @OneToMany(mappedBy = "article", cascade = CascadeType.ALL)
    private List<Annotation> annotations;
    @ManyToMany(mappedBy = "articles", cascade = CascadeType.ALL)
    private List<Author> authors;
    @ManyToOne
    Corrector corrector;
    @OneToMany(mappedBy = "article", cascade = CascadeType.ALL)
    @OrderBy("datetime desc")
    List<Version> versions;

    public Article() {
    }

    public Article(Long id, String pathOfFile, String name, Boolean priority, Status status, List<Annotation> annotations, List<Author> authors, Corrector corrector) {
        this.id = id;
        this.name = name;
        this.priority = priority;
        this.status = status;
        this.annotations = annotations;
        this.authors = authors;
        this.corrector = corrector;
    }

    public List<Annotation> getAnnotations() {
        return annotations;
    }

    public void setAnnotations(List<Annotation> annotations) {
        this.annotations = annotations;
    }

    public void addAnnotation(Annotation annotation) {
        if (this.annotations == null) {
            annotations = new ArrayList<Annotation>();
        }
        if (!this.annotations.contains(annotation)) {
            annotations.add(annotation);
        }
    }

    //corrector
    public Corrector getCorrector() {
        return corrector;
    }

    public void setCorrector(Corrector corrector) {
        this.corrector = corrector;
        corrector.addArticle(this);
    }

    //author
    public List<Author> getAuthors() {
        return authors;
    }

    public void setAuthors(List<Author> authors) {
        this.authors = authors;
    }

    public void addAuthor(Author author) {
        if (this.authors == null) {
            authors = new ArrayList<Author>();
        }
        if (!this.authors.contains(author)) {
            authors.add(author);
        }
    }

    public void removeAuthor(Author author) {
        if (this.authors == null) {
            return;
        }
        this.authors.remove(author);


    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getPriority() {
        return priority;
    }

    public void setPriority(Boolean priority) {
        this.priority = priority;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public Boolean isPriority() {
        return priority;
    }

    @OrderBy("datetime desc")
    public List<Version> getVersions() {
        return versions;
    }

    public void setVersions(List<Version> versions) {
        this.versions = versions;
    }

    public void addVersion(Version version) {
        if (this.versions == null) {
            versions = new ArrayList<Version>();
        }
        if (!this.versions.contains(version)) {
            versions.add(version);
        }
    }

//    public ArticleDto getDto() {
//        ArticleDto dto = new ArticleDto();
//        dto.setId(this.getId());
//        dto.setAnnotationsIDs(DtoTransformerHelper.getIdentifiers(this.getAnnotations()));
//        dto.setAuthorsIDs(DtoTransformerHelper.getIdentifiers(this.getAuthors()));
//        dto.setCorrectorID(HibernateTools.getIdentifier(this.getCorrector()));
//        dto.setName(this.getName());
//        dto.setPriority(this.getPriority());
//        dto.setStatus(this.getStatus());
//        dto.setVersions(DtoTransformerHelper.getIdentifiers(this.getVersions()));
//        return dto;
//    }
}

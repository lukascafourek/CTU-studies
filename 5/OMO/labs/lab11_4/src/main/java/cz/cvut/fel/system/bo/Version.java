/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bo;

import cz.cvut.fel.system.dto.VersionDto;
import cz.cvut.fel.system.helper.HibernateTools;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;

/**
 *
 * @author marcel
 */
@Entity
public class Version extends DomainEntity {

    @Temporal(javax.persistence.TemporalType.TIMESTAMP)
    Date datetime;
    @Column(length = 1024)
    String pathOfFile;
    @ManyToOne
    Article article;

    public Date getDatetime() {
        return datetime;
    }

    public void setDatetime(Date datetime) {
        this.datetime = datetime;
    }

    public String getPathOfFile() {
        return pathOfFile;
    }

    public void setPathOfFile(String pathOfFile) {
        this.pathOfFile = pathOfFile;
    }

    public Article getArticle() {
        return article;
    }

    public void setArticle(Article article) {
        this.article = article;
    }

//    public VersionDto getDto() {
//        Version a = this;
//        VersionDto dto = new VersionDto();
//        dto.setArticleID(HibernateTools.getIdentifier(this.getArticle()));
//        dto.setDatetime(this.getDatetime());
//        dto.setId(this.getId());
//        dto.setPathOfFile(this.getPathOfFile());
//        return dto;
//    }
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.dto;

import java.util.Date;

/**
 *
 * @author marcel
 */
public class VersionDto extends AbstractDto {

    private Long articleID;
    private Date datetime;
    private String pathOfFile;

    public VersionDto() {
    }

    public Long getArticleID() {
        return articleID;
    }

    public void setArticleID(Long articleIDs) {
        this.articleID = articleIDs;
    }

    public Date getDatetime() {
        return datetime;
    }

    public void setDatetime(Date datetime) {
        this.datetime = datetime;
    }

    public String getPathOfFile() {
        return pathOfFile;
    }

    public void setPathOfFile(String PathOfFile) {
        this.pathOfFile = PathOfFile;
    }

    @Override
    public String toString() {
        return "VersionDto{" + "articleID=" + articleID + ", datetime=" + datetime + ", pathOfFile=" + pathOfFile + '}';
    }
    
    
}

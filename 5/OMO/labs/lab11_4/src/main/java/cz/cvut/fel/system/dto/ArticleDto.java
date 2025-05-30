/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.dto;

import cz.cvut.fel.system.bo.Status;
import java.util.List;

/**
 *
 * @author Jirka
 */
public class ArticleDto extends AbstractDto {

    private String name;
    private Boolean priority;
    private Status status;
    private List<Long> annotationsIDs;
    private List<Long> authorsIDs;
    private Long correctorID;
    private List<Long> versions;

    public ArticleDto() {
    }

    public List<Long> getVersions() {
        return versions;
    }

    public void setVersions(List<Long> versions) {
        this.versions = versions;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean isPriority() {
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

    public List<Long> getAnnotationsIDs() {
        return annotationsIDs;
    }

    public void setAnnotationsIDs(List<Long> annotationsIDs) {
        this.annotationsIDs = annotationsIDs;
    }

    public List<Long> getAuthorsIDs() {
        return authorsIDs;
    }

    public void setAuthorsIDs(List<Long> authorsIDs) {
        this.authorsIDs = authorsIDs;
    }

    public Long getCorrectorID() {
        return correctorID;
    }

    public void setCorrectorID(Long correctorID) {
        this.correctorID = correctorID;
    }
}

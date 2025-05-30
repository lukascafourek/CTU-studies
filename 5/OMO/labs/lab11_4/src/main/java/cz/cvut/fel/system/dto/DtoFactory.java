/*
 * To change a template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.dto;

import cz.cvut.fel.system.bo.Admin;
import cz.cvut.fel.system.bo.AdvisoryBoardMember;
import cz.cvut.fel.system.bo.Annotation;
import cz.cvut.fel.system.bo.Article;
import cz.cvut.fel.system.bo.Author;
import cz.cvut.fel.system.bo.Corrector;
import cz.cvut.fel.system.bo.User;
import cz.cvut.fel.system.bo.Version;
import cz.cvut.fel.system.helper.DtoTransformerHelper;
import cz.cvut.fel.system.helper.HibernateTools;

/**
 *
 * @author marcel
 */
public  class DtoFactory {

    public static AdminDto getDto(Admin a) {
        AdminDto dto = new AdminDto();
        dto.setId(a.getId());
        dto.setRole(a.getUserRole());

        dto.setUsername(a.getUsername());
        return dto;
    }

    public static AdvisoryBoardMemberDto getDto(AdvisoryBoardMember a) {
        AdvisoryBoardMemberDto dto = new AdvisoryBoardMemberDto();
        dto.setId(a.getId());
        dto.setFirstname(a.getFirstname());
        dto.setLastname(a.getLastname());
        dto.setRole(a.getUserRole());
        dto.setUsername(a.getUsername());
        return dto;
    }

    public static AnnotationDto getDto(Annotation a) {
        AnnotationDto dto = new AnnotationDto();
        dto.setAnnotation(a.getAnnotationText());
        dto.setArticleId(HibernateTools.getIdentifier(a.getArticle()));
        dto.setAuthorOfAnnotationId(HibernateTools.getIdentifier(a.getAuthorOfAnnotation()));
        dto.setId(a.getId());
        dto.setPathOfFile(a.getPathOfFile());
        return dto;
    }

    public static ArticleDto getDto(Article a) {
        ArticleDto dto = new ArticleDto();
        dto.setId(a.getId());
        dto.setAnnotationsIDs(DtoTransformerHelper.getIdentifiers(a.getAnnotations()));
        dto.setAuthorsIDs(DtoTransformerHelper.getIdentifiers(a.getAuthors()));
        dto.setCorrectorID(HibernateTools.getIdentifier(a.getCorrector()));
        dto.setName(a.getName());
        dto.setPriority(a.getPriority());
        dto.setStatus(a.getStatus());
        dto.setVersions(DtoTransformerHelper.getIdentifiers(a.getVersions()));
        return dto;
    }

    public static AuthorDto getDto(Author a) {
        AuthorDto dto = new AuthorDto();
        dto.setFirstname(a.getFirstname());
        dto.setLastname(a.getLastname());
        dto.setId(a.getId());
        dto.setRole(a.getUserRole());
        dto.setUsername(a.getUsername());
        dto.setArticleIds(DtoTransformerHelper.getIdentifiers(a.getArticles()));
        return dto;
    }

    public static CorrectorDto getDto(Corrector a) {
        CorrectorDto dto = new CorrectorDto();
        dto.setId(a.getId());
        dto.setRole(a.getUserRole());
        dto.setFirstname(a.getFirstname());
        dto.setLastname(a.getLastname());
        dto.setUsername(a.getUsername());
        dto.setArticleIds(DtoTransformerHelper.getIdentifiers(a.getArticles()));
        dto.setEmail(a.getEmail());

        return dto;
    }

    public static UserDto getDto(User a) {
        UserDto dto = new UserDto();
        dto.setId(a.getId());
        dto.setRole(a.getUserRole());
        dto.setUsername(a.getUsername());
        return dto;
    }

    public static VersionDto getDto(Version a) {
        VersionDto dto = new VersionDto();
        dto.setArticleID(HibernateTools.getIdentifier(a.getArticle()));
        dto.setDatetime(a.getDatetime());
        dto.setId(a.getId());
        dto.setPathOfFile(a.getPathOfFile());
        return dto;
    }
}

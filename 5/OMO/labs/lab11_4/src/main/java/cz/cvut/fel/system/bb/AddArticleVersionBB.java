/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import static cz.cvut.fel.system.bb.ArticleBB.LOCATION;
import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.service.ArticleServiceImpl;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import javax.annotation.PostConstruct;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.ServletContext;
import org.apache.commons.io.FilenameUtils;
import org.primefaces.model.UploadedFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 *
 * @author marcel
 */
@Component
@Scope("request")
public class AddArticleVersionBB {

//@ManagedProperty(name="articleService", value="#{articleService}" )
    @Autowired
    protected ArticleServiceImpl articleService;
    @Value("#{request.getParameter('articleId')}")
    private String articleId;
    protected String name;
    protected Long articleIdLong;
    protected UploadedFile file;
    @Autowired
    ArticleDetailBB articleDetailBB;

    @PostConstruct
    public void init() {
        if (articleId == null) {
            return;
        }

        articleIdLong = Long.parseLong(articleId.trim());
        ArticleDto adto = articleService.getArticleById(articleIdLong);
        this.name = adto.getName();

    }

    public String addVersion(String articleId) {
        this.articleId = articleId;
        init();
        try {

            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            String relativeWebPath = "\\resources\\files\\";//"C:\\wpauploads\\"
            ServletContext servletContext = (ServletContext) externalContext.getContext();
            String absoluteDiskPath = servletContext.getRealPath(relativeWebPath);
            LOCATION = new File(absoluteDiskPath);
        } catch (Exception e) {
            System.out.println(e);
        }


        /*  String filename = file.getFileName().toString();
         InputStream input = file.getInputstream();
         OutputStream output = new FileOutputStream(new File("/path/to/uploads", filename));
         */
        try {

            if (file != null) {
                String prefix = FilenameUtils.getBaseName(file.getFileName());
                String suffix = FilenameUtils.getExtension(file.getFileName());
                if (!LOCATION.exists()) {
                    System.out.println("creating directory");
                    LOCATION.mkdir();
                }


                File save = File.createTempFile("article", "." + suffix, LOCATION);
                Files.write(save.toPath(), file.getContents());
                // Add success message here.
                File nameFile = new File(save.getAbsolutePath());
                String nameFileStr = nameFile.getName();
                articleService.addVersionOfArticle(articleIdLong, nameFileStr);

            }
        } catch (Exception e) {
            System.out.println(e);
        }
        //add status
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

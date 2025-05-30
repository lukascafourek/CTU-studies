/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.fel.system.bb;

import cz.cvut.fel.system.bo.Role;
import cz.cvut.fel.system.bo.Status;
import cz.cvut.fel.system.dto.ArticleDto;
import cz.cvut.fel.system.dto.AuthorDto;
import cz.cvut.fel.system.dto.CorrectorDto;
import cz.cvut.fel.system.dto.UserDto;
import cz.cvut.fel.system.dto.VersionDto;
import cz.cvut.fel.system.service.ArticleServiceImpl;
import cz.cvut.fel.system.service.UserServiceImpl;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.SelectItem;
import javax.servlet.ServletContext;
import org.apache.commons.io.FilenameUtils;
import org.primefaces.model.UploadedFile;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

/**
 *
 * @author Jirka
 */
@Component
@Scope("request")
public class ArticleBB {

//@ManagedProperty(name="articleService", value="#{articleService}" )
    @Autowired
    protected ArticleServiceImpl articleService;
    @Autowired
    protected UserServiceImpl userServiceImpl;
    protected String name;
    protected boolean priority;
    protected Status status;
    protected UploadedFile file;
    List<String> authors;
    String selectedCorrector;
    String pathOfFile;
    private HtmlDataTable datatableArticles;
    protected String message;

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
    

    public HtmlDataTable getDatatableArticles() {
        return datatableArticles;
    }

    public void setDatatableArticles(HtmlDataTable datatableArticles) {
        this.datatableArticles = datatableArticles;
    }
//file
//protected String file;
    /**
     * getters and setters *
     */
    protected static File LOCATION = null;
// action

    public String saveArticle() throws IOException {
        //get id of Author that is logged in
        List<Long> userIds = null;
        //getting user ID
        Object user = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (user instanceof UserDetails) {
            String username = ((UserDetails) user).getUsername();
            UserDto udto = userServiceImpl.getUserByUsername(username);
            if (udto.getRole() == Role.ROLE_AUTHOR) {
                userIds = new ArrayList<Long>();
                userIds.add(udto.getId());
            }
        }



        ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
        String relativeWebPath = "\\resources\\files\\";//"C:\\wpauploads\\"
        ServletContext servletContext = (ServletContext) externalContext.getContext();
        String absoluteDiskPath = servletContext.getRealPath(relativeWebPath);
        LOCATION = new File(absoluteDiskPath);


        /*  String filename = file.getFileName().toString();
         InputStream input = file.getInputstream();
         OutputStream output = new FileOutputStream(new File("/path/to/uploads", filename));
         */
        if (file != null) {
            String prefix = FilenameUtils.getBaseName(file.getFileName());
            String suffix = FilenameUtils.getExtension(file.getFileName());
            System.out.println("Location" + LOCATION.getAbsolutePath());
            if (!LOCATION.exists()) {
                System.out.println("creating directory");
                LOCATION.mkdir();
            }
            File save = File.createTempFile("article", "." + suffix, LOCATION);
            System.out.println("save" + save.getAbsolutePath());
            Files.write(save.toPath(), file.getContents());
            // Add success message here.
            File nameFile = new File(save.getAbsolutePath());
            String nameFileStr = nameFile.getName();
            articleService.addArticle(nameFileStr, name, priority, userIds);

        } else {
            //inserting to database
            articleService.addArticle(null, name, priority, userIds);
        }
        //add status
        message="***Article added***";
        return "/index";
    }

    public ArticleServiceImpl getArticleService() {
        return articleService;
    }

    public void setArticleService(ArticleServiceImpl articles) {
        this.articleService = articles;
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

    public UploadedFile getFile() {
        return file;
    }

    public void setFile(UploadedFile file) {
        this.file = file;

    }

    public SelectItem[] getStatusValues() {
        SelectItem[] items = new SelectItem[Status.values().length];
        int i = 0;
        for (Status g : Status.values()) {
            items[i++] = new SelectItem(g, g.getLabel());
        }
        return items;
    }

    public SelectItem[] getAllCorrectors() {
        List<CorrectorDto> allCorrectors = userServiceImpl.getAllCorrectors();
        SelectItem[] items = new SelectItem[allCorrectors.size() + 1];
        items[0] = new SelectItem(0, "do not change");

        int i = 1;
        for (CorrectorDto g : allCorrectors) {
            items[i++] = new SelectItem(g.getId(), g.getFirstname() + " " + g.getLastname());
        }
        return items;
    }

    public boolean isPriority() {
        return priority;
    }

    public void setPriority(boolean priority) {
        this.priority = priority;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public String getPathOfFile() {
        return pathOfFile;
    }

    public void setPathOfFile(long id) {
        List<VersionDto> list = articleService.getAllVersionsOfArticle(id);
        if(list!= null && !list.isEmpty()){
        VersionDto v = list.get(0);
        this.pathOfFile = v.getPathOfFile();
        }
        else{
        this.message="No version of article available";
        }
    }

    public List<String> getAuthors() {
        List<AuthorDto> authorsTemp = userServiceImpl.getAllAuthors();
        for (int i = 0; i < authorsTemp.size(); i++) {
            this.authors.add(authorsTemp.get(i).getUsername());
        }
        return authors;
    }

    public void setAuthors(String[] authors) {
    }

    public String getSelectedCorrector() {
        return selectedCorrector;
    }

    public void setSelectedCorrector(String selectedCorrector) {
        this.selectedCorrector = selectedCorrector;
    }

    public void selectedCorrectorChange(ValueChangeEvent e) {
        System.out.println(e.getSource());
        System.out.println(e.getNewValue());
        CorrectorDto cd = userServiceImpl.getCorrectorById(Long.parseLong((String) e.getNewValue()));
        int index = datatableArticles.getRowIndex();
        ArticleDto ardto = (ArticleDto) datatableArticles.getRowData();
        articleService.setCorrectorOfArticle(ardto.getId(), Long.parseLong((String) e.getNewValue()));
        System.out.println(index);
    }

    public String correctorOfArticle(Long correctorId) {
        System.out.println(correctorId);
        if (correctorId == null || correctorId == 0) {
            return "-";
        }
        CorrectorDto corr = userServiceImpl.getCorrectorById(correctorId);
        String r = corr.getFirstname() + " " + corr.getLastname();
        return r;
    }
}

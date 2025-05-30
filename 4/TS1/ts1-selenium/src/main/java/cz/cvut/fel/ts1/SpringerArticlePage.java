package cz.cvut.fel.ts1;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;
import java.util.List;

public class SpringerArticlePage {

    @FindBy(className = "c-article-title")
    private WebElement articleName;

    @FindBy(css = ".c-bibliographic-information__value time")
    private WebElement articleDate;

    @FindBy(className = "c-bibliographic-information__value")
    private List<WebElement> articleMetadata;

    public SpringerArticlePage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public String getArticleName() {
        return articleName.getText();
    }

    public String getArticleDate() {
        return articleDate.getAttribute("datetime");
    }

    public String getArticleDOI() {
        for (WebElement metadataItem : articleMetadata) {
            String text = metadataItem.getText();
            if (text.startsWith("https://")) {
                return text;
            }
        }
        return null;
    }
}

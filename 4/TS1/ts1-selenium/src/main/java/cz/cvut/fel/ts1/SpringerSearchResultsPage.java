package cz.cvut.fel.ts1;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class SpringerSearchResultsPage {

    @FindBy(xpath = "//*[@id=\"list-content-type-filter\"]/li[2]/div/label")
    private WebElement articleContentTypeButton;

    @FindBy(css = "#date-from")
    private WebElement dateFrom;

    @FindBy(css = "#date-to")
    private WebElement dateTo;

    @FindBy(css = "#popup-filters > div.app-filter-buttons > button.eds-c-button.eds-c-button--primary")
    private WebElement filterButton;

    @FindBy(css = "#main > div > div:nth-child(4) > div > div:nth-child(2) > div:nth-child(2) > ol")
    private WebElement articleResultsList;

    @FindBy(css = "body > div.app-elements.u-mb-16 > header > div > div > a")
    private WebElement logoButton;

    public SpringerSearchResultsPage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public void filterByArticles() {
        articleContentTypeButton.click();
        dateFrom.sendKeys("2024");
        dateTo.sendKeys("2024");
        filterButton.click();
    }

    public void clickArticleTitle(int x) {
        WebElement article = articleResultsList.findElement
                (By.xpath("//*[@id=\"main\"]/div/div[3]/div/div[2]/div[2]/ol/li["+x+"]/div/div/h3/a"));
        article.click();
    }

    public void clickLogoButton() {
        logoButton.click();
    }
}

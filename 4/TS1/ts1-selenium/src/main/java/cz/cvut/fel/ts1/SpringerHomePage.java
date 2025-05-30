package cz.cvut.fel.ts1;

import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class SpringerHomePage {

    @FindBy(id = "identity-account-widget")
    private WebElement loginButton;

    @FindBy(id = "homepage-search")
    private WebElement searchBar;

    public SpringerHomePage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public void clickLoginButton() {
        loginButton.click();
    }

    public void searchForArticle(String articleName) {
        searchBar.sendKeys(articleName);
        searchBar.sendKeys(Keys.ENTER);
    }
}

package cz.cvut.fel.ts1;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import java.time.Duration;

public class LoginPage {

    private final WebDriver driver;

    @FindBy(xpath = "//*[@id=\"sso-form\"]/a")
    private WebElement sso;

    public LoginPage(WebDriver driver) {
        this.driver = driver;
        PageFactory.initElements(driver, this);
    }

    public void clickSSO() {
        sso.click();
    }

    public void clickLoginInfo(String username, String password) {
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
        WebElement usernameField = wait.until(
                ExpectedConditions.elementToBeClickable(By.xpath("//*[@id=\"username\"]")));
        usernameField.sendKeys(username);
        WebElement passwordField = wait.until(
                ExpectedConditions.elementToBeClickable(By.xpath("//*[@id=\"password\"]")));
        passwordField.sendKeys(password);
        WebElement loginButton = wait.until(
                ExpectedConditions.elementToBeClickable(By.xpath
                        ("//*[@id=\"cvut-login-form\"]/form/fieldset/div/div/div/div[5]/button")));
        loginButton.click();
    }

}

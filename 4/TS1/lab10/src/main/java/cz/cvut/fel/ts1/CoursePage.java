package cz.cvut.fel.ts1;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import java.time.Duration;

public class CoursePage {

    private final WebDriver driver;

    @FindBy(xpath = "//*[@id=\"module-308513\"]/div/div[2]/div/div/div/div/a")
    private WebElement testModule;

    public CoursePage(WebDriver driver) {
        this.driver = driver;
        PageFactory.initElements(driver, this);
    }

    public void clickTestModule() {
        testModule.click();
    }

    public void clickAttempt() {
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
        WebElement attempt = wait.until(ExpectedConditions.elementToBeClickable
                (By.cssSelector("[id*='single_button']")));
        attempt.click();
        WebElement start = wait.until(ExpectedConditions.elementToBeClickable
                (By.xpath("//*[@id=\"id_submitbutton\"]")));
        start.click();
    }

    public void clickUserLogin() {
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
        WebElement actionMenu = wait.until(ExpectedConditions.elementToBeClickable
                (By.xpath("//*[@id=\"action-menu-toggle\"]")));
        actionMenu.click();
        WebElement logOut = wait.until(ExpectedConditions.elementToBeClickable
                (By.xpath("//*[@id=\"actionmenuaction-6\"]")));
        logOut.click();
        WebElement continueButton = wait.until(ExpectedConditions.elementToBeClickable
                (By.cssSelector("[id*='single_button']")));
        continueButton.click();
    }

}

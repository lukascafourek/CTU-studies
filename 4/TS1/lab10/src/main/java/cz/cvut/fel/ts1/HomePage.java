package cz.cvut.fel.ts1;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import java.time.Duration;

public class HomePage {

    private final WebDriver driver;

    @FindBy(xpath = "//*[@id=\"navbarSupportedContent\"]/div/a/span")
    private WebElement login;

    public HomePage(WebDriver driver) {
        this.driver = driver;
        PageFactory.initElements(driver, this);
    }

    public void clickLogin() {
        login.click();
    }

    public void clickCourse() {
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(1));
        WebElement course = wait.until(ExpectedConditions.elementToBeClickable(By.xpath
                ("//*[@id=\"mykos-courses-list\"]/div[18]/div/a")));
        course.click();
    }

}

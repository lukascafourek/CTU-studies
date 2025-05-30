package cz.cvut.fel.ts1;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class SpringerLoginEmailPage {
    @FindBy(id = "login-email")
    private WebElement emailBar;

    @FindBy(id = "email-submit")
    private WebElement emailButton;

    public SpringerLoginEmailPage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public void enterEmail(String email) {
        emailBar.sendKeys(email);
        emailButton.click();
    }
}

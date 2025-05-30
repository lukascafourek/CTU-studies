package cz.cvut.fel.ts1;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;

public class SpringerLoginPasswordPage {
    @FindBy(id = "login-password")
    private WebElement passwordBar;

    @FindBy(id = "password-submit")
    private WebElement passwordButton;

    public SpringerLoginPasswordPage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public void enterPassword(String password) {
        passwordBar.sendKeys(password);
        passwordButton.click();
    }
}

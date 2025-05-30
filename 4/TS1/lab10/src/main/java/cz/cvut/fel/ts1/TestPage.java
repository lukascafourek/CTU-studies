package cz.cvut.fel.ts1;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.FindBy;
import org.openqa.selenium.support.PageFactory;
import java.util.List;

public class TestPage {

    @FindBy(css = "textarea[id*='1_answer_id']")
    private WebElement firstAnswer;

    @FindBy(css = "input[id*='2_answer']")
    private WebElement secondAnswer;

    @FindBy(css = "select[id*='3_p1']")
    private WebElement thirdAnswer;

    @FindBy(css = "select[id*='4_p1']")
    private WebElement fourthAnswer;

    public TestPage(WebDriver driver) {
        PageFactory.initElements(driver, this);
    }

    public void submitFirstAnswer(String answer) {
        firstAnswer.sendKeys(answer);
    }

    public void submitSecondAnswer(String answer) {
        secondAnswer.sendKeys(answer);
    }

    public void submitThirdAnswer() {
        thirdAnswer.click();
        List<WebElement> options = thirdAnswer.findElements(By.tagName("option"));
        for (WebElement option : options) {
            if (option.getText().contains("Oberon")) {
                option.click();
                return;
            }
        }
    }

    public void submitFourthAnswer() {
        fourthAnswer.click();
        List<WebElement> options = fourthAnswer.findElements(By.tagName("option"));
        for (WebElement option : options) {
            if (option.getText().contains("Rumunsko")) {
                option.click();
                return;
            }
        }
    }

}

package cz.cvut.fel.ts1;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import java.time.Duration;

public class Main {

    public static void main(String[] args) {
        WebDriver driver = new ChromeDriver();
        driver.get("https://moodle.fel.cvut.cz/");
        driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(1));
        driver.manage().window().maximize();
        driver.manage().deleteAllCookies();

        HomePage homePage = new HomePage(driver);
        homePage.clickLogin();

        LoginPage loginPage = new LoginPage(driver);
        loginPage.clickSSO();
        loginPage.clickLoginInfo("username", "password");

        homePage.clickCourse();

        CoursePage coursePage = new CoursePage(driver);
        coursePage.clickTestModule();
        coursePage.clickAttempt();

        TestPage testPage = new TestPage(driver);
        testPage.submitFirstAnswer("Lukáš Cafourek 104");
        testPage.submitSecondAnswer("86400");
        testPage.submitThirdAnswer();
        testPage.submitFourthAnswer();

        String testURL = driver.getCurrentUrl();
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(20));
        wait.until(ExpectedConditions.not(ExpectedConditions.urlToBe(testURL)));

        coursePage.clickUserLogin();

        driver.close();
    }

}

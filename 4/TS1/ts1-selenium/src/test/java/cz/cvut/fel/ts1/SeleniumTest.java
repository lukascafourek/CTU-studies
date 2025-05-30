package cz.cvut.fel.ts1;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import java.time.Duration;
import java.util.ArrayList;
import java.util.stream.Stream;

public class SeleniumTest {

    private static WebDriver driver;

    private static final ArrayList<Article> articleList = new ArrayList<>();

    @BeforeAll
    public static void setup() {
        driver = new ChromeDriver();
        driver.manage().timeouts().implicitlyWait(Duration.ofSeconds(10));
        driver.get("https://link.springer.com");
        driver.manage().window().maximize();
        driver.manage().deleteAllCookies();
        performSearchAndExtractArticles();
    }

    private static void performSearchAndExtractArticles() {
        SpringerHomePage homePage = new SpringerHomePage(driver);
        homePage.searchForArticle("Page AND Object AND Model AND (Selenium OR Testing)");

        SpringerSearchResultsPage resultsPage = new SpringerSearchResultsPage(driver);
        resultsPage.filterByArticles();

        for (int i = 0; i < 4; i++) {
            resultsPage.clickArticleTitle(i + 1);

            SpringerArticlePage articlePage = new SpringerArticlePage(driver);
            String name = articlePage.getArticleName();
            String date = articlePage.getArticleDate();
            String doi = articlePage.getArticleDOI();

            articleList.add(new Article(name, date, doi));

            driver.navigate().back();
        }

        resultsPage.clickLogoButton();

        homePage = new SpringerHomePage(driver);
        homePage.clickLoginButton();

        SpringerLoginEmailPage emailPage = new SpringerLoginEmailPage(driver);
        emailPage.enterEmail("username");

        SpringerLoginPasswordPage passwordPage = new SpringerLoginPasswordPage(driver);
        passwordPage.enterPassword("password");   // cannot give real password due to safety reasons
    }

    private static Stream<Arguments> provideArticles() {
        return articleList.stream().map(Arguments::of);
    }

    @ParameterizedTest
    @MethodSource("provideArticles")
    public void testSearch(Article article) {
        SpringerHomePage homePage = new SpringerHomePage(driver);
        homePage.searchForArticle(article.getName());

        SpringerSearchResultsPage resultsPage = new SpringerSearchResultsPage(driver);
        resultsPage.clickArticleTitle(1);

        SpringerArticlePage articlePage = new SpringerArticlePage(driver);

        Assertions.assertEquals(article.getName(), articlePage.getArticleName());
        Assertions.assertEquals(article.getDate(), articlePage.getArticleDate());
        Assertions.assertEquals(article.getDoi(), articlePage.getArticleDOI());

        driver.navigate().back();
        driver.navigate().back();
    }

    @AfterAll
    public static void teardown() {
        driver.quit();
    }
}

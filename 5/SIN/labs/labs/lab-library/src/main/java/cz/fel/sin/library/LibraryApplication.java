package cz.fel.sin.library;

import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.repository.*;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.Objects;

@EnableCaching
@EnableScheduling
@SpringBootApplication
public class LibraryApplication implements CommandLineRunner {

    private static final Logger log = LoggerFactory.getLogger(LibraryApplication.class);

    @Autowired
    private BookRepository bookRepository;

    @Autowired
    private CacheManager cacheManager;

    public static void main(String[] args) {
        SpringApplication.run(LibraryApplication.class, args);
    }

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI().info(new Info()
                .title("Library")
                .description("Demo library application")
                .version("1.0.0")
        );
    }

    @Scheduled(fixedRate = 1000 * 3600 * 2) // every 2 hours
    public void evictAllcachesAtIntervals() {
        cacheManager.getCacheNames().forEach(cacheName -> Objects.requireNonNull(cacheManager.getCache(cacheName)).clear());
    }

    @Override
    public void run(String... args) {

        log.info("StartApplication...");

//        Book book1 = new Book();
//        book1.setName("Java");
//        book1.setISBN("1");
//        Book book2 = new Book();
//        book2.setName("Node");
//        book2.setISBN("2");
//        Book book3 = new Book();
//        book3.setName("Python");
//        book3.setISBN("3");
//        bookRepository.save(book1);
//        bookRepository.save(book2);
//        bookRepository.save(book3);
//
//        System.out.println("\nfindAll()");
//        bookRepository.findAll().forEach(System.out::println);
//
//        System.out.println("\nfindById(1)");
//        bookRepository.findById(1).ifPresent(System.out::println);
//
//        System.out.println("\nfindByName('Node')");
//        bookRepository.findByName("Node").forEach(System.out::println);
    }
}

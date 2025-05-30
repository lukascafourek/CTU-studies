package sin.library.libraryapp.controller.mock;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MockController {

    @GetMapping("/mock/book/{id}")
    public ResponseEntity<String> getBookById(@PathVariable("id") Integer id) {
        String book = """
                {%n\
                    "id": %d,%n\
                    "isbn": "978-3-16-148410-0",%n\
                    "name": "The Book",%n\
                    "published": "2021-01-01",%n\
                    "publisher": {%n\
                        "id": 1,%n\
                        "name": "Publisher"%n\
                    },%n\
                    "genre": {%n\
                        "id": 1,%n\
                        "name": "Genre"%n\
                    },%n\
                    "library": {%n\
                        "id": 1,%n\
                        "name": "Library"%n\
                    },%n\
                    "authors": [%n\
                        {%n\
                            "id": 1,%n\
                            "firstName": "John",%n\
                            "surName": "Doe",%n\
                            "email": "email@email.com"%n\
                        }%n\
                    ]%n\
                }%n\
                """.formatted(id);
        return ResponseEntity.ok(book);
    }

    @GetMapping("/mock/library/{id}")
    public ResponseEntity<String> getLibraryById(@PathVariable("id") Integer id) {
        String library = """
                {%n\
                    "id": %d,%n\
                    "name": "Library",%n\
                    "address": {%n\
                        "id": 1,%n\
                        "street": "Street",%n\
                        "houseNumber": "1"%n\
                    },%n\
                    "books": [%n\
                        {%n\
                            "id": 1,%n\
                            "isbn": "978-3-16-148410-0",%n\
                            "name": "The Book",%n\
                            "published": "2021-01-01"%n\
                        }%n\
                    ]%n\
                }%n\
                """.formatted(id);
        return ResponseEntity.ok(library);
    }

    @GetMapping("/mock/publisher/{id}")
    public ResponseEntity<String> getPublisherById(@PathVariable("id") Integer id) {
        String publisher = """
                {%n\
                    "id": %d,%n\
                    "name": "Publisher",%n\
                    "address": {%n\
                        "id": 1,%n\
                        "street": "Street",%n\
                        "houseNumber": "1"%n\
                    },%n\
                    "contracts": [%n\
                        {%n\
                            "id": 1,%n\
                            "firstName": "John",%n\
                            "surName": "Doe",%n\
                            "email": "email@email.com"%n\
                        }%n\
                    ],%n\
                    "books": [%n\
                        {%n\
                            "id": 1,%n\
                            "isbn": "978-3-16-148410-0",%n\
                            "name": "The Book",%n\
                            "published": "2021-01-01"%n\
                        }%n\
                    ]%n\
                }%n\
                """.formatted(id);
        return ResponseEntity.ok(publisher);
    }
}

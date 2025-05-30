package cz.fel.sin.library.controller.mock;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MockResponseController {

    @GetMapping("/mock/books/{id}")
    public String getMockBookById(@PathVariable Integer id) {
        return """
                {
                    "id": %d,
                    "title": "Mocked Book Title",
                    "isbn": "123-456-789",
                    "genre": {
                        "id": 1,
                        "name": "Mock Genre"
                    },
                    "authors": [
                        {
                            "id": 1,
                            "firstName": "John",
                            "surName": "Doe"
                        }
                    ]
                }
                """.formatted(id);
    }

    @GetMapping("/mock/publishers/{id}")
    public String getMockPublisherById(@PathVariable Integer id) {
        return """
                {
                    "id": %d,
                    "name": "Mocked Publisher Name",
                    "address": "123 Mock St."
                }
                """.formatted(id);
    }
}

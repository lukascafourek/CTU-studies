package sin.library.libraryapp.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.service.BookService;

@RestController("/book")
@RequiredArgsConstructor
public class BookController {

    private final BookService bookService;

    @GetMapping("/book/{id}")
    public ResponseEntity<BookDto> getBookById(@PathVariable("id") Integer id) {
        return ResponseEntity.ok(bookService.findById(id));
    }
}

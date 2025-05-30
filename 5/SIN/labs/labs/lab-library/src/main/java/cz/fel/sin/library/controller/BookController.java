package cz.fel.sin.library.controller;

import cz.fel.sin.library.dto.BookDTO;
import cz.fel.sin.library.dto.DTOMapperImpl;
import cz.fel.sin.library.service.BookService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController("/book")
@RequiredArgsConstructor
public class BookController {

    private final BookService bookService;
    private final DTOMapperImpl dtoMapper;

    @GetMapping("/book/{id}")
    public ResponseEntity<BookDTO> getBookById(@PathVariable("id") Integer id) {
        return ResponseEntity.ok(dtoMapper.bookToDto(bookService.findById(id)));
    }
}

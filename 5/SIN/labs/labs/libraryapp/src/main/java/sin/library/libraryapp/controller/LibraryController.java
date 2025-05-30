package sin.library.libraryapp.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import sin.library.libraryapp.dto.LibraryDto;
import sin.library.libraryapp.service.LibraryService;

@RestController("/library")
@RequiredArgsConstructor
public class LibraryController {

    private final LibraryService libraryService;

    @GetMapping("/library/{id}")
    public ResponseEntity<LibraryDto> getLibraryById(@PathVariable("id") Integer id) {
        return ResponseEntity.ok(libraryService.findById(id));
    }

    @PatchMapping("/library/{libraryId}/book/{bookId}")
    public ResponseEntity<String> addBookToLibrary(@PathVariable("libraryId") Integer libraryId, @PathVariable("bookId") Integer bookId) {
        if (libraryService.addBookToLibrary(libraryId, bookId)) {
            return ResponseEntity.ok("Book added to library");
        } else {
            return ResponseEntity.badRequest().body("Book not added to library");
        }
    }
}

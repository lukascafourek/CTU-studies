package cz.fel.sin.library.controller;

import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.model.Library;
import cz.fel.sin.library.service.LibraryService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController("/library")
@RequiredArgsConstructor
public class LibraryController {

    private final LibraryService libraryService;

    @GetMapping("/library/{id}")
    public ResponseEntity<Library> getLibraryById(@PathVariable("id") Integer id) {
        return ResponseEntity.ok(libraryService.findById(id));
    }

    @PatchMapping("/library/{libraryId}/book/{bookId}")
    public ResponseEntity<String> addBookToLibrary(@PathVariable("libraryId") Integer libraryId, @PathVariable("bookId") Integer bookId) {
        try {
            boolean result = libraryService.addBookToLibrary(libraryId, bookId);
            if (result) {
                return ResponseEntity.ok("Book added to library successfully.");
            } else {
                return ResponseEntity.badRequest().body("Failed to add book to library.");
            }
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
}

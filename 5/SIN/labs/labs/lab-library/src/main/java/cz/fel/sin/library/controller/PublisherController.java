package cz.fel.sin.library.controller;

import cz.fel.sin.library.dto.request.BookCreate;
import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.service.PublisherService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController("/publisher")
@RequiredArgsConstructor
public class PublisherController {

    private final PublisherService publisherService;

    @PostMapping("/publisher/{id}/book")
    public ResponseEntity<String> publishNewBook(@PathVariable("id") Integer publisherId, @RequestBody BookCreate bookCreate) {
        try {
            int bookId = publisherService.publishNewBook(publisherId, bookCreate);
            if (bookId > 0) {
                return ResponseEntity.ok("Book published successfully.");
            } else {
                return ResponseEntity.badRequest().body("Failed to publish the book.");
            }
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @PatchMapping("/publisher/{publisherId}/author/{authorId}")
    public ResponseEntity<String> createContract(@PathVariable("authorId") Integer authorId, @PathVariable("publisherId") Integer publisherId) {
        try {
            boolean success = publisherService.createContract(authorId, publisherId);
            if (success) {
                return ResponseEntity.ok("Contract created successfully.");
            } else {
                return ResponseEntity.badRequest().body("Failed to create the contract.");
            }
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
}

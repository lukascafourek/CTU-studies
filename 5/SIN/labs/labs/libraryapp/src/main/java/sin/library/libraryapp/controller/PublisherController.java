package sin.library.libraryapp.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.dto.PublisherDto;
import sin.library.libraryapp.service.PublisherService;

@RestController("/publisher")
@RequiredArgsConstructor
public class PublisherController {

    private final PublisherService publisherService;

    @GetMapping("/publisher/{id}")
    public ResponseEntity<PublisherDto> getPublisherById(@PathVariable("id") Integer id) {
        return ResponseEntity.ok(publisherService.findById(id));
    }

    @PatchMapping("/publisher/{publisherId}/author/{authorId}")
    public ResponseEntity<String> createContract(@PathVariable("publisherId") Integer publisherId, @PathVariable("authorId") Integer authorId) {
        if (publisherService.createContract(authorId, publisherId)) {
            return ResponseEntity.ok("Contract created");
        } else {
            return ResponseEntity.badRequest().body("Contract not created");
        }
    }

    @PostMapping("/publisher/{id}/book")
    public ResponseEntity<Integer> publishNewBook(@PathVariable("id") Integer publisherId, @RequestBody BookDto bookDto) {
        return ResponseEntity.ok(publisherService.publishNewBook(publisherId, bookDto));
    }
}

package cz.fel.sin.library.service;

import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.model.Library;
import cz.fel.sin.library.repository.BookRepository;
import cz.fel.sin.library.repository.LibraryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@RequiredArgsConstructor
@Service
@Log4j2
public class LibraryServiceImpl implements LibraryService {

    private final LibraryRepository libraryRepository;
    private final BookRepository bookRepository;

    @Override
    public Library findById(Integer id) {
        log.info("findById entered with id: {}", id);
        if (id == null)
            throw new FieldMissingException("ID_IS_NULL");
        log.info("FindById exited with id: {}", id);
        return libraryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("LIBRARY_NOT_FOUND"));
    }

    @Override
    public boolean addBookToLibrary(Integer libraryId, Integer bookId) {
        log.info("addBookToLibrary entered with libraryId: {}, bookId: {}", libraryId, bookId);
        if (libraryId == null)
            throw new FieldMissingException("LIBRARY_ID_IS_NULL");
        if (bookId == null)
            throw new FieldMissingException("BOOK_ID_IS_NULL");
        Library library = libraryRepository.findById(libraryId)
                .orElseThrow(() -> new NotFoundException("LIBRARY_NOT_FOUND"));
        Book book = bookRepository.findById(bookId)
                .orElseThrow(() -> new NotFoundException("BOOK_NOT_FOUND"));
        library.getBooks().add(book);
        libraryRepository.save(library);
        log.info("addBookToLibrary exited with libraryId: {}, bookId: {}, return: true", libraryId, bookId);
        return true;
    }
}

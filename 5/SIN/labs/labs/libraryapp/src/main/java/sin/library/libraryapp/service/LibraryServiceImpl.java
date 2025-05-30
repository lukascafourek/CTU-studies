package sin.library.libraryapp.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import sin.library.libraryapp.dto.LibraryDto;
import sin.library.libraryapp.exception.NotFoundException;
import sin.library.libraryapp.mapper.LibraryMapper;
import sin.library.libraryapp.model.Book;
import sin.library.libraryapp.model.Library;
import sin.library.libraryapp.repository.BookRepository;
import sin.library.libraryapp.repository.LibraryRepository;

@Service
@RequiredArgsConstructor
@Log4j2
public class LibraryServiceImpl implements LibraryService {

    private final LibraryRepository libraryRepository;
    private final BookRepository bookRepository;
    private final LibraryMapper libraryMapper;

    public LibraryDto findById(Integer id) {
        return libraryMapper.toDto(libraryRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Library with id %d was not found".formatted(id))));
    }

    public boolean addBookToLibrary(Integer libraryId, Integer bookId) {
        log.info("addBookToLibrary entered with libraryId: {}, bookId: {}", libraryId, bookId);
        Library library = libraryRepository.findById(libraryId)
                .orElseThrow(() -> new NotFoundException("Library with id %d was not found".formatted(libraryId)));
        Book book = bookRepository.findById(bookId)
                .orElseThrow(() -> new NotFoundException("Book with id %d was not found".formatted(bookId)));
        library.getBooks().add(book);
        libraryRepository.save(library);
        log.info("addBookToLibrary exited with libraryId: {}, bookId: {}, return: true", libraryId, bookId);
        return true;
    }
}

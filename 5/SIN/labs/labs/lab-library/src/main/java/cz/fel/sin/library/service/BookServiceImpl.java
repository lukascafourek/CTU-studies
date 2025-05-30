package cz.fel.sin.library.service;

import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.repository.BookRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@RequiredArgsConstructor
@Service
@Log4j2
public class BookServiceImpl implements BookService {

    private final BookRepository bookRepository;

    @Override
    public Book findById(Integer id){
        log.info("findById entered with id: {}", id);
        if (id == null)
            throw new FieldMissingException("ID_IS_NULL");
        log.info("FindById exited with id: {}", id);
        return bookRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("BOOK_NOT_FOUND"));
    }
}

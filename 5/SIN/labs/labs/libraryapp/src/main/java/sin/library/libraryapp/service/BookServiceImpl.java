package sin.library.libraryapp.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.exception.FieldMissingException;
import sin.library.libraryapp.mapper.BookMapper;
import sin.library.libraryapp.repository.BookRepository;

@Service
@RequiredArgsConstructor
@Log4j2
public class BookServiceImpl implements BookService {

    private final BookRepository bookRepository;
    private final BookMapper bookMapper;

    @Override
    public BookDto findById(Integer id) {
        return bookMapper.toDto(bookRepository.findById(id)
                .orElseThrow(() -> new FieldMissingException("Book with id %d was not found".formatted(id))));
    }
}

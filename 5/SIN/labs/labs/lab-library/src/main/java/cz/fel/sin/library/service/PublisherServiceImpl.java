package cz.fel.sin.library.service;

import cz.fel.sin.library.dto.RequestMapper;
import cz.fel.sin.library.dto.request.BookCreate;
import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Author;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.model.Genre;
import cz.fel.sin.library.model.Publisher;
import cz.fel.sin.library.repository.AuthorRepository;
import cz.fel.sin.library.repository.BookRepository;
import cz.fel.sin.library.repository.GenreRepository;
import cz.fel.sin.library.repository.PublisherRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@RequiredArgsConstructor
@Service
@Log4j2
public class PublisherServiceImpl implements PublisherService {

    private final PublisherRepository publisherRepository;

    private final BookRepository bookRepository;

    private final AuthorRepository authorRepository;

    private final GenreRepository genreRepository;

    private final RequestMapper requestMapper;

    @Override
    public int publishNewBook(Integer publisherId, BookCreate bookCreate) {
        log.info("publishNewBook entered with id {}", publisherId);
        if (publisherId == null)
            throw new FieldMissingException("PUBLISHER_ID_IS_NULL");
        Publisher publisher = publisherRepository.findById(publisherId)
                .orElseThrow(() -> new NotFoundException("PUBLISHER_NOT_FOUND"));
        if (publisher.getBooks().stream()
                .anyMatch(b -> Objects.equals(b.getISBN(), bookCreate.getIsbn())))
            throw new FieldInvalidException("BOOK_ALREADY_EXISTS");
        Book book = requestMapper.createToBook(bookCreate);
        if (book == null)
            throw new RuntimeException("ERROR_CREATING_NEW_BOOK");
        book.setPublisher(publisher);
        Genre genre = genreRepository.findById(bookCreate.getGenre())
                .orElseThrow(() -> new NotFoundException("GENRE_NOT_FOUND"));
        book.setGenre(genre);
        List<Author> authors = authorRepository.findAllById(bookCreate.getAuthors());
        book.setAuthor(authors);
        bookRepository.save(book);
        log.info("publishNewBook exited with publisherId: {}, bookId: {}", publisherId, book.getId());
        return book.getId();
    }

    @Override
    public boolean createContract(Integer authorId, Integer publisherId) {
        log.info("createContract entered with authorId {} and publisherId {}", authorId, publisherId);
        if (authorId == null)
            throw new FieldMissingException("AUTHOR_ID_IS_NULL");
        if (publisherId == null)
            throw new FieldMissingException("PUBLISHER_ID_IS_NULL");
        Author author = authorRepository.findById(authorId)
                .orElseThrow(() -> new NotFoundException("AUTHOR_NOT_FOUND"));
        Publisher publisher = publisherRepository.findById(publisherId)
                .orElseThrow(() -> new NotFoundException("PUBLISHER_NOT_FOUND"));
        if (publisher.getContracts().stream()
                .anyMatch(c -> Objects.equals(c.getId(), authorId)))
            throw new FieldInvalidException("CONTRACT_ALREADY_EXISTS");
        publisher.getContracts().add(author);
        publisherRepository.save(publisher);
        log.info("createContract exited with authorId: {}, publisherId: {}, return: true", authorId, publisherId);
        return true;
    }
}

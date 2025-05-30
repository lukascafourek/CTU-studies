package sin.library.libraryapp.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.dto.PublisherDto;
import sin.library.libraryapp.exception.FieldInvalidException;
import sin.library.libraryapp.exception.NotFoundException;
import sin.library.libraryapp.mapper.BookMapper;
import sin.library.libraryapp.mapper.PublisherMapper;
import sin.library.libraryapp.model.*;
import sin.library.libraryapp.repository.*;

import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class PublisherServiceImpl implements PublisherService {

    private final PublisherRepository publisherRepository;
    private final AuthorRepository authorRepository;
    private final BookRepository bookRepository;
    private final GenreRepository genreRepository;
    private final LibraryRepository libraryRepository;
    private final PublisherMapper publisherMapper;
    private final BookMapper bookMapper;

    public PublisherDto findById(Integer id) {
        return publisherMapper.toDto(publisherRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("Publisher with id %d was not found".formatted(id))));
    }

    public boolean createContract(Integer publisherId, Integer authorId) {
        log.info("createContract entered with publisherId: {}, authorId: {}", publisherId, authorId);
        Publisher publisher = publisherRepository.findById(publisherId)
                .orElseThrow(() -> new NotFoundException("Publisher with id %d was not found".formatted(publisherId)));
        Author author = authorRepository.findById(authorId)
                .orElseThrow(() -> new NotFoundException("Author with id %d was not found".formatted(authorId)));
        if (publisher.getContracts().stream()
                .anyMatch(a -> a.getId().equals(authorId))) {
            throw new FieldInvalidException("Author already has a contract with this publisher");
        }
        log.info("Creating contract between publisher {} and author {}", publisherId, authorId);
        publisher.getContracts().add(author);
        author.getPublishers().add(publisher);
        publisherRepository.save(publisher);
        authorRepository.save(author);
        return true;
    }

    public Integer publishNewBook(Integer publisherId, BookDto bookDto) {
        log.info("publishNewBook entered with id {}", publisherId);
        Publisher publisher = publisherRepository.findById(publisherId)
                .orElseThrow(() -> new NotFoundException("Publisher with id %d was not found".formatted(publisherId)));
        if (publisher.getBooks().stream()
                .anyMatch(b -> b.getId().equals(bookDto.getId()))) {
            throw new FieldInvalidException("Book already exists in publisher's library");
        }
        Book book = bookMapper.toEntity(bookDto);
        if (book == null) {
            throw new RuntimeException("Error creating new book");
        }
        book.setPublisher(publisher);
        book.setLibrary(libraryRepository.findById(bookDto.getLibraryId())
                .orElseThrow(() -> new NotFoundException("Library with id %d was not found".formatted(bookDto.getLibraryId()))));
        book.setGenre(genreRepository.findById(bookDto.getGenreId())
                .orElseThrow(() -> new NotFoundException("Genre with id %d was not found".formatted(bookDto.getGenreId()))));
        book.setAuthors(authorRepository.findAllById(bookDto.getAuthorsIds()));
        book = bookRepository.save(book);
        publisher.getBooks().add(book);
        publisherRepository.save(publisher);
//        Library library = libraryRepository.findById(bookDto.getLibraryId())
//                .orElseThrow(() -> new NotFoundException("Library with id %d was not found".formatted(bookDto.getLibraryId())));
//        library.getBooks().add(book);
//        libraryRepository.save(library);
//        Genre genre = genreRepository.findById(bookDto.getGenreId())
//                .orElseThrow(() -> new NotFoundException("Genre with id %d was not found".formatted(bookDto.getGenreId())));
//        genre.getBooks().add(book);
//        genreRepository.save(genre);
//        List<Author> authors = authorRepository.findAllById(bookDto.getAuthorsIds());
//        Book finalBook = book;
//        authors.forEach(author -> author.getBooks().add(finalBook));
//        authorRepository.saveAll(authors);
        log.info("publishNewBook exited with publisherId: {}, bookId: {}", publisherId, book.getId());
        return book.getId();
    }
}

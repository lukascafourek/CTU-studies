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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@ExtendWith(MockitoExtension.class)
class PublisherServicePublishTest {

    @MockBean
    private PublisherRepository publisherRepository;

    @MockBean
    private GenreRepository genreRepository;

    @MockBean
    private BookRepository bookRepository;

    @MockBean
    private AuthorRepository authorRepository;

    @MockBean
    private RequestMapper requestMapper;

    @SpyBean
    private PublisherServiceImpl publisherService;

    private BookCreate bookCreate;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        bookCreate = new BookCreate();
        bookCreate.setName("Book");
        bookCreate.setIsbn("1234567890");
        bookCreate.setPublished(LocalDate.now());
        bookCreate.setGenre(1);
        bookCreate.setAuthors(List.of(1, 2));
    }

    @Test
    void publishNewBook_success() {
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setBooks(new ArrayList<>());
        Book book = new Book();
        book.setId(1);
        book.setISBN("1234567890");

        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));
        when(genreRepository.findById(1)).thenReturn(Optional.of(new Genre()));
        when(requestMapper.createToBook(bookCreate)).thenReturn(book);

        int bookId = publisherService.publishNewBook(1, bookCreate);

        assertEquals(1, bookId);
        verify(bookRepository, times(1)).save(book);
    }

    @Test
    void publishNewBook_publisherIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> publisherService.publishNewBook(null, bookCreate));
    }

    @Test
    void publishNewBook_publisherNotFound_throwsNotFoundException() {
        when(publisherRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> publisherService.publishNewBook(1, bookCreate));
    }

    @Test
    void publishNewBook_bookAlreadyExists_throwsFieldInvalidException() {
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setBooks(new ArrayList<>());
        Book existingBook = new Book();
        existingBook.setISBN("1234567890");
        publisher.getBooks().add(existingBook);

        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));

        assertThrows(FieldInvalidException.class, () -> publisherService.publishNewBook(1, bookCreate));
    }

    @Test
    void publishNewBook_errorCreatingNewBook_throwsRuntimeException() {
        Publisher publisher = new Publisher();
        publisher.setId(1);

        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));
        when(requestMapper.createToBook(bookCreate)).thenReturn(null);

        assertThrows(RuntimeException.class, () -> publisherService.publishNewBook(1, bookCreate));
    }

    @Test
    void publishNewBook_genreNotFound_throwsNotFoundException() {
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setBooks(new ArrayList<>());

        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));
        when(genreRepository.findById(1)).thenReturn(Optional.empty());
        when(requestMapper.createToBook(bookCreate)).thenReturn(new Book());

        assertThrows(NotFoundException.class, () -> publisherService.publishNewBook(1, bookCreate));
    }

    @Test
    void publishNewBook_successWithAuthors() {
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setBooks(new ArrayList<>());
        Book book = new Book();
        book.setId(1);
        book.setISBN("1234567890");
        List<Author> authors = List.of(new Author(), new Author());

        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));
        when(requestMapper.createToBook(bookCreate)).thenReturn(book);
        when(genreRepository.findById(1)).thenReturn(Optional.of(new Genre()));
        when(authorRepository.findAllById(bookCreate.getAuthors())).thenReturn(authors);

        int bookId = publisherService.publishNewBook(1, bookCreate);

        assertEquals(1, bookId);
        verify(bookRepository, times(1)).save(book);
    }
}

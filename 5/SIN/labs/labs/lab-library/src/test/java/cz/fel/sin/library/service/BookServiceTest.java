package cz.fel.sin.library.service;

import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.repository.BookRepository;
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

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@ExtendWith(MockitoExtension.class)
class BookServiceTest {

    @MockBean
    private BookRepository bookRepository;

    @SpyBean
    private BookServiceImpl service;

    private Book testBook;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);

        // Create a mock book for testing
        testBook = new Book();
        testBook.setId(1);
        testBook.setName("Test Book");
    }

    @Test
    void findById_existingBook_bookRetrieved() {
        // Arrange
        when(bookRepository.findById(testBook.getId())).thenReturn(Optional.of(testBook));

        // Act
        Book foundBook = service.findById(testBook.getId());

        // Assert
        assertNotNull(foundBook);
        assertEquals(testBook.getId(), foundBook.getId());
        assertEquals(testBook.getName(), foundBook.getName());
    }

    @Test
    void findById_nullId_throwsFieldMissingException() {
        // Act & Assert
        FieldMissingException exception = assertThrows(FieldMissingException.class, () -> service.findById(null));

        assertEquals("ID_IS_NULL", exception.getMessage());
    }

    @Test
    void findById_nonExistingBook_throwsNotFoundException() {
        // Arrange
        when(bookRepository.findById(999)).thenReturn(Optional.empty());

        // Act & Assert
        NotFoundException exception = assertThrows(NotFoundException.class, () -> service.findById(999));

        assertEquals("BOOK_NOT_FOUND", exception.getMessage());
    }
}

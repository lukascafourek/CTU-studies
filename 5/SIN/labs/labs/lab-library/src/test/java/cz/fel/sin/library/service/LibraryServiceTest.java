package cz.fel.sin.library.service;

import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.model.Library;
import cz.fel.sin.library.repository.BookRepository;
import cz.fel.sin.library.repository.LibraryRepository;
import org.junit.jupiter.api.Assertions;
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

import java.util.ArrayList;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@ExtendWith(MockitoExtension.class)
class LibraryServiceTest {

    @MockBean
    private LibraryRepository libraryRepository;

    @MockBean
    private BookRepository bookRepository;

    @SpyBean
    private LibraryServiceImpl libraryService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void findById_libraryExists_returnsLibrary() {
        Library library = new Library();
        library.setId(1);
        when(libraryRepository.findById(1)).thenReturn(Optional.of(library));

        Library result = libraryService.findById(1);

        Assertions.assertNotNull(result);
        Assertions.assertEquals(1, result.getId());
    }

    @Test
    void findById_libraryIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> libraryService.findById(null));
    }

    @Test
    void findById_libraryNotFound_throwsNotFoundException() {
        when(libraryRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> libraryService.findById(1));
    }

    @Test
    void addBookToLibrary_bookExistsInLibrary_returnsTrue() {
        Library library = new Library();
        library.setId(1);
        library.setBooks(new ArrayList<>());
        Book book = new Book();
        book.setId(1);
        when(libraryRepository.findById(1)).thenReturn(Optional.of(library));
        when(bookRepository.findById(1)).thenReturn(Optional.of(book));

        boolean result = libraryService.addBookToLibrary(1, 1);

        assertTrue(result);
        verify(libraryRepository, times(1)).save(library);
    }

    @Test
    void addBookToLibrary_bookIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> libraryService.addBookToLibrary(1, null));
    }

    @Test
    void addBookToLibrary_libraryIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> libraryService.addBookToLibrary(null, 1));
    }

    @Test
    void addBookToLibrary_libraryNotFound_throwsNotFoundException() {
        when(libraryRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> libraryService.addBookToLibrary(1, 1));
    }

    @Test
    void addBookToLibrary_bookNotFound_throwsNotFoundException() {
        Library library = new Library();
        library.setId(1);
        when(libraryRepository.findById(1)).thenReturn(Optional.of(library));
        when(bookRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> libraryService.addBookToLibrary(1, 1));
    }
}

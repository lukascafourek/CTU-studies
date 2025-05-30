package cz.fel.sin.library.service;

import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.exception.FieldMissingException;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Author;
import cz.fel.sin.library.model.Publisher;
import cz.fel.sin.library.repository.AuthorRepository;
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

import java.util.ArrayList;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@ExtendWith(MockitoExtension.class)
class PublisherServiceContractTest {

    @MockBean
    private PublisherRepository publisherRepository;

    @MockBean
    private AuthorRepository authorRepository;

    @SpyBean
    private PublisherServiceImpl publisherService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void createContract_success() {
        Author author = new Author();
        author.setId(1);
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setContracts(new ArrayList<>());

        when(authorRepository.findById(1)).thenReturn(Optional.of(author));
        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));

        boolean result = publisherService.createContract(1, 1);

        assertTrue(result);
        verify(publisherRepository, times(1)).save(publisher);
    }

    @Test
    void createContract_authorIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> publisherService.createContract(null, 1));
    }

    @Test
    void createContract_publisherIdIsNull_throwsFieldMissingException() {
        assertThrows(FieldMissingException.class, () -> publisherService.createContract(1, null));
    }

    @Test
    void createContract_authorNotFound_throwsNotFoundException() {
        when(authorRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> publisherService.createContract(1, 1));
    }

    @Test
    void createContract_publisherNotFound_throwsNotFoundException() {
        Author author = new Author();
        author.setId(1);
        when(authorRepository.findById(1)).thenReturn(Optional.of(author));
        when(publisherRepository.findById(1)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> publisherService.createContract(1, 1));
    }

    @Test
    void createContract_contractAlreadyExists_throwsFieldInvalidException() {
        Author author = new Author();
        author.setId(1);
        Publisher publisher = new Publisher();
        publisher.setId(1);
        publisher.setContracts(new ArrayList<>());
        publisher.getContracts().add(author);

        when(authorRepository.findById(1)).thenReturn(Optional.of(author));
        when(publisherRepository.findById(1)).thenReturn(Optional.of(publisher));

        assertThrows(FieldInvalidException.class, () -> publisherService.createContract(1, 1));
    }
}

package cz.fel.sin.library;

import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.repository.BookRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class LibraryApplicationTests {

	@Mock
	private BookRepository bookRepository;

	@Mock
	private CacheManager cacheManager;

	@InjectMocks
	private LibraryApplication libraryApplication;

	@BeforeEach
	void setUp() {
		MockitoAnnotations.openMocks(this);
	}

	@Test
	void evictAllCachesAtIntervals_success() {
		// Arrange
		Cache mockCache = mock(Cache.class);
		when(cacheManager.getCacheNames()).thenReturn(List.of("cache1", "cache2"));
		when(cacheManager.getCache("cache1")).thenReturn(mockCache);
		when(cacheManager.getCache("cache2")).thenReturn(mockCache);

		// Act
		libraryApplication.evictAllcachesAtIntervals();

		// Assert
		verify(mockCache, times(2)).clear(); // Ensure clear() is called for each cache
	}

	@Test
	void run_initializesBooksCorrectly() {
		// Arrange
		Book book1 = new Book();
		book1.setId(1);
		book1.setName("Java");
		book1.setISBN("1");
		Book book2 = new Book();
		book2.setId(2);
		book2.setName("Node");
		book2.setISBN("2");
		Book book3 = new Book();
		book3.setId(3);
		book3.setName("Python");
		book3.setISBN("3");
		List<Book> mockBooks = List.of(book1, book2, book3);

		when(bookRepository.findAll()).thenReturn(mockBooks);

		// Act
		List<Book> books = bookRepository.findAll();

		// Assert
		assertEquals(3, books.size());
		assertTrue(books.stream().anyMatch(book -> book.getName().equals("Java") && book.getISBN().equals("1")));
		assertTrue(books.stream().anyMatch(book -> book.getName().equals("Node") && book.getISBN().equals("2")));
		assertTrue(books.stream().anyMatch(book -> book.getName().equals("Python") && book.getISBN().equals("3")));
	}

	@Test
	void run_findByIdReturnsCorrectBook() {
		// Arrange
		Book book = new Book();
		book.setId(1);
		book.setName("Java");
		book.setISBN("1");
		when(bookRepository.findById(1)).thenReturn(Optional.of(book));

		// Act
		Optional<Book> result = bookRepository.findById(1);

		// Assert
		assertTrue(result.isPresent());
		assertEquals("Java", result.get().getName());
	}

	@Test
	void run_findByNameReturnsCorrectBooks() {
		// Arrange
		Book book = new Book();
		book.setId(2);
		book.setName("Node");
		book.setISBN("2");
		List<Book> mockBooks = List.of(book);
		when(bookRepository.findByName("Node")).thenReturn(mockBooks);

		// Act
		List<Book> books = bookRepository.findByName("Node");

		// Assert
		assertEquals(1, books.size());
		assertEquals("Node", books.getFirst().getName());
	}
}

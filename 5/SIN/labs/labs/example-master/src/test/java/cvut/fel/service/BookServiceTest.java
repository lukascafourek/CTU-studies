package cvut.fel.service;

import cvut.fel.entity.Book;
import cvut.fel.exception.NotFoundException;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import static org.junit.Assert.*;

@RunWith(SpringRunner.class)
@Transactional
@SpringBootTest()
public class BookServiceTest {

	@PersistenceContext
	private EntityManager em;

	@Autowired
	private BookService service;

	@BeforeEach
	public void setUp(){
	}

	@Test
	public void findById_existingBook_bookRetrieved(){
		// Arrange
		Book book = generateBook();

		// Act
		Book foundBook = service.findById(book.getId());

		//Assert
		assertSame(book, foundBook);
	}

	@Test(expected = NotFoundException.class)
	public void findById_wrongBookId_expectException(){
		// Arrange
		Long bookId = Long.MIN_VALUE;

		// Act
		Book foundBook = service.findById(bookId);

		fail("Exception not thrown.");
	}

	private Book generateBook(){
		Book book = new Book("name");
		em.persist(book);
		return book;
	}

}

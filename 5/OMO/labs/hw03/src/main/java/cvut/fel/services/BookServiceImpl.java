package cvut.fel.services;

import cvut.fel.dao.BookRepository;
import cvut.fel.model.Book;
import cvut.fel.services.strategy.BookStrategy;
import cvut.fel.services.strategy.SimpleBookStrategy;

public class BookServiceImpl {

    BookRepository bookRepository = new BookRepository();
    BookStrategy bookStrategy = new SimpleBookStrategy();

    public void setBookStrategy(BookStrategy bookStrategy) {
        this.bookStrategy = bookStrategy;
        bookStrategy.setBookRepository(bookRepository);
    }

    public Book getByBookId(int bookId) {
        //more logic for books in here
        return bookRepository.getById(bookId);
    }

    public Book getByBookName(String name) {
        //more logic for books in here
        return bookRepository.getByName(name);
    }

    public Book updateBook(Book book) {
        return bookStrategy.update(book);
    }
}

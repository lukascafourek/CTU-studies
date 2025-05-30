package cvut.fel.services.strategy;

import cvut.fel.dao.BookRepository;
import cvut.fel.model.Book;

public class SimpleBookStrategy implements BookStrategy {

    private BookRepository bookRepository;

    @Override
    public void setBookRepository(BookRepository bookRepository) {
        this.bookRepository = bookRepository;
    }

    @Override
    public Book update(Book book) {
        return bookRepository.update(book);
    }
}

package cvut.fel.services.strategy;

import cvut.fel.dao.BookRepository;
import cvut.fel.model.Book;

public interface BookStrategy {

    void setBookRepository(BookRepository bookRepository);

    Book update (Book book);
}

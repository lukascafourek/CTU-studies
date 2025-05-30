package cvut.fel.services.strategy;

import cvut.fel.dao.BookRepository;
import cvut.fel.model.Book;

public class ImutableBookStrategy implements BookStrategy {

    private BookRepository bookRepository;

    @Override
    public void setBookRepository(BookRepository bookRepository) {
        this.bookRepository = bookRepository;
    }

    @Override
    public Book update(Book book) {
        Book newBook = new Book(book.getId(), book.getISBN(), book.getName());
        newBook.incrementId();
        bookRepository.save(newBook);
        return newBook;
    }
}

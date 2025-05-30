package cvut.fel.dao;

import cvut.fel.model.Book;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

public class BookRepository {

    List <Book> books  = new ArrayList<Book>();
    public BookRepository() {
        books.add(new Book( 1, 99921l,"book1"));
        books.add(new Book( 2, 99922l,"book2"));
        books.add(new Book( 3, 99923l,"book3"));

    }

    public Book getById(int id) {
        for (Book book : books) {
            if (book.getId() == id) {
                return book;
            }
        }
        throw new NoSuchElementException();
    }

    public Book getByName(String name) {
        for (Book book : books) {
            if (book.getName().equals(name)) {
                return book;
            }
        }
        throw new NoSuchElementException();
    }
    public Book update(Book bookInput) {
        for (Book book : books){
            if(book.getId() == bookInput.getId()){
                books.remove(book);
                books.add(bookInput);
                return bookInput;
            }
        }
        throw new RuntimeException("not found book for update");
    }
    public void save(Book bookInput) {
        books.add(bookInput);
    }
}


package cvut.fel.facade.mapper;

import cvut.fel.facade.dto.BookDto;
import cvut.fel.model.Author;
import cvut.fel.model.Book;
import cvut.fel.model.Library;

public class BookMapper {
    public static BookDto toDto(Book book, Author author, Library library){
        return new BookDto(book.getName(), author.getFirstname(), library.getName());
    }
}

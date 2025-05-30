package cvut.fel.facade;

import cvut.fel.facade.dto.BookDto;
import cvut.fel.facade.mapper.BookMapper;
import cvut.fel.services.AuthorServiceImpl;
import cvut.fel.services.BookServiceImpl;
import cvut.fel.services.LibraryServiceImpl;

public class BookFacadeImpl {

    private AuthorServiceImpl authorService;
    private BookServiceImpl bookService;
    private LibraryServiceImpl libraryService;

    public BookFacadeImpl() {
        authorService = new AuthorServiceImpl();
        bookService = new BookServiceImpl();
        libraryService = new LibraryServiceImpl();
    }

    public BookFacadeImpl(AuthorServiceImpl authorService, BookServiceImpl bookService, LibraryServiceImpl libraryService) {
        this.authorService = authorService;
        this.bookService = bookService;
        this.libraryService = libraryService;
    }

    public BookDto getByBookId(int id) {
        return BookMapper.toDto(bookService.getByBookId(id), authorService.getByBookId(id), libraryService.getByBookId(id));
    }
}

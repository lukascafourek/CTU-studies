package cvut.fel.services;

import cvut.fel.dao.AuthorRepository;
import cvut.fel.dao.LibraryRepository;
import cvut.fel.model.Author;
import cvut.fel.model.Library;

public class AuthorServiceImpl {
    AuthorRepository authorRepository = new AuthorRepository();
    public Author getByBookId(int bookId) {
        //more logic for books in here
        return authorRepository.getByBookId(bookId);
    }
}

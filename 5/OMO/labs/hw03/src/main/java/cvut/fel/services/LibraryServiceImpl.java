package cvut.fel.services;

import cvut.fel.dao.LibraryRepository;
import cvut.fel.model.Library;

public class LibraryServiceImpl {
    LibraryRepository libraryRepository = new LibraryRepository();
    public Library getByBookId(int bookId) {
        //more logic for books in here
        return libraryRepository.getByBookId(bookId);
    }
}

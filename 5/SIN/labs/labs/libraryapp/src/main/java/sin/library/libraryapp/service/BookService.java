package sin.library.libraryapp.service;

import sin.library.libraryapp.dto.BookDto;

public interface BookService {

    BookDto findById(Integer id);
}

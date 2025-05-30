package sin.library.libraryapp.service;

import sin.library.libraryapp.dto.LibraryDto;

public interface LibraryService {

    LibraryDto findById(Integer id);

    boolean addBookToLibrary(Integer libraryId, Integer bookId);
}

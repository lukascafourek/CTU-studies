package cz.fel.sin.library.service;

import cz.fel.sin.library.model.Library;

public interface LibraryService {

    Library findById(Integer id);

    boolean addBookToLibrary(Integer libraryId, Integer bookId);
}

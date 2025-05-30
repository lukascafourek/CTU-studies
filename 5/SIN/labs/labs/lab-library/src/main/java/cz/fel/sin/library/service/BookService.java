package cz.fel.sin.library.service;

import cz.fel.sin.library.model.Book;

public interface BookService {

    Book findById(Integer id);
}

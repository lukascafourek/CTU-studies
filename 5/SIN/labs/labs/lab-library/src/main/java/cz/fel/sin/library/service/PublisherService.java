package cz.fel.sin.library.service;

import cz.fel.sin.library.dto.request.BookCreate;

public interface PublisherService {

    int publishNewBook(Integer publisherId, BookCreate bookCreate);

    boolean createContract(Integer authorId, Integer publisherId);
}

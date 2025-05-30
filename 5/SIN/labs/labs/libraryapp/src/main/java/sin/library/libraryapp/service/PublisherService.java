package sin.library.libraryapp.service;

import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.dto.PublisherDto;

public interface PublisherService {

    PublisherDto findById(Integer id);

    boolean createContract(Integer publisherId, Integer authorId);

    Integer publishNewBook(Integer publisherId, BookDto bookDto);
}

package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Book;

public interface BookRepository extends JpaRepository<Book, Integer> {
}
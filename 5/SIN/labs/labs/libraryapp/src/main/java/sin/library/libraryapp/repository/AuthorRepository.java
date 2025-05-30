package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Author;

public interface AuthorRepository extends JpaRepository<Author, Integer> {
}
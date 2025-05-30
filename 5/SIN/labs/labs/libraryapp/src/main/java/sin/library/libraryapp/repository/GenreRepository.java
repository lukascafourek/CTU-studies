package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Genre;

public interface GenreRepository extends JpaRepository<Genre, Integer> {
}
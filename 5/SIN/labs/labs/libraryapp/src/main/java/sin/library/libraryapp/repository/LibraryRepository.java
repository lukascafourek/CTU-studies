package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Library;

public interface LibraryRepository extends JpaRepository<Library, Integer> {
}
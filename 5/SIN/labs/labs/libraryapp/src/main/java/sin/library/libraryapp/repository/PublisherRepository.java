package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Publisher;

public interface PublisherRepository extends JpaRepository<Publisher, Integer> {
}
package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.City;

public interface CityRepository extends JpaRepository<City, Integer> {
}
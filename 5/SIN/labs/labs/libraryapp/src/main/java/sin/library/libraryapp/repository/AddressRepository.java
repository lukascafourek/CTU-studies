package sin.library.libraryapp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import sin.library.libraryapp.model.Address;

public interface AddressRepository extends JpaRepository<Address, Integer> {
}
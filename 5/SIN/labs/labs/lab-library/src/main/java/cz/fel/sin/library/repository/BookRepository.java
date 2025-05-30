package cz.fel.sin.library.repository;

import cz.fel.sin.library.model.Book;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface BookRepository extends JpaRepository<Book, Integer> {

    @Query("SELECT CASE WHEN COUNT(b) > 0 THEN true ELSE false END FROM Book b WHERE b.ISBN = :ISBN")
    boolean existsByISBN(@Param("ISBN") String ISBN);

    @Query("SELECT b FROM Book b WHERE b.name = :name")
    List<Book> findByName(@Param("name") String name);
}

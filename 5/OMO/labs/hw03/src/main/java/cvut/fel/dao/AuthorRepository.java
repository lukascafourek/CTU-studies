package cvut.fel.dao;

import cvut.fel.model.Address;
import cvut.fel.model.Author;
import cvut.fel.model.Book;
import cvut.fel.model.Library;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class AuthorRepository {

    List <Author> authors  = new ArrayList<Author>();
    public AuthorRepository() {
        authors.add(new Author( 1, "firstname1","surname1",new Address("street1", 1, "city1"), 1));
    }

    public Author getByBookId(int id) {
        return authors.stream().filter(a -> a.getBook() == id).findFirst().get();
    }

}


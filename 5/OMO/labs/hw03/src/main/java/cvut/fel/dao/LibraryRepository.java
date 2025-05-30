package cvut.fel.dao;

import cvut.fel.model.Address;
import cvut.fel.model.Book;
import cvut.fel.model.Library;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class LibraryRepository {

    List <Library> libraries  = new ArrayList<Library>();
    public LibraryRepository() {
        libraries.add(new Library( 1, "library1",new Address("street1", 1, "city1"),
                new ArrayList<Integer>(Arrays.asList(1,2,3))));
        libraries.add(new Library( 2, "library1",new Address("street2", 2, "city2"),
                new ArrayList<Integer>(Arrays.asList(1,2,3))));
    }

    public Library getByBookId(int id) {
        boolean found = false;
        for(Library library : libraries) {
            for(Integer bookId : library.getBooksIds()) {
                if(bookId == id) {
                    //found it!
                    found = true;
                    break;
                }
            }
            if (found) {
                return library;
            }
        }
        throw new RuntimeException("not found");
    }

}


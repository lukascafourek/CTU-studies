package sin.library.libraryapp.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@Data
public class BookDto {

    Integer id;
    String isbn;
    String name;
    LocalDate published;
    Integer publisherId;
    Integer genreId;
    Integer libraryId;
    List<Integer> authorsIds;

    public List<Integer> getAuthorsIds() {
        if (authorsIds != null) {
            return new ArrayList<>(authorsIds);
        }
        return null;
    }

    public void setAuthorsIds(List<Integer> authorsIds) {
        if (authorsIds != null) {
            this.authorsIds = new ArrayList<>(authorsIds);
        }
    }
}

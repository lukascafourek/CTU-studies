package sin.library.libraryapp.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class LibraryDto {

    Integer id;
    String name;
    Integer addressId;
    List<Integer> bookIds;

    public List<Integer> getBookIds() {
        if (bookIds != null) {
            return new ArrayList<>(bookIds);
        }
        return null;
    }

    public void setBookIds(List<Integer> bookIds) {
        if (bookIds != null) {
            this.bookIds = new ArrayList<>(bookIds);
        }
    }
}

package sin.library.libraryapp.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class AuthorDto {

    Integer id;
    String firstName;
    String surName;
    String email;
    Integer addressId;
    List<Integer> booksIds;
    List<Integer> publishersIds;

    public List<Integer> getBooksIds() {
        if (booksIds != null) {
            return new ArrayList<>(booksIds);
        }
        return null;
    }

    public void setBooksIds(List<Integer> booksIds) {
        if (booksIds != null) {
            this.booksIds = new ArrayList<>(booksIds);
        }
    }

    public List<Integer> getPublishersIds() {
        if (publishersIds != null) {
            return new ArrayList<>(publishersIds);
        }
        return null;
    }

    public void setPublishersIds(List<Integer> publishersIds) {
        if (publishersIds != null) {
            this.publishersIds = new ArrayList<>(publishersIds);
        }
    }
}

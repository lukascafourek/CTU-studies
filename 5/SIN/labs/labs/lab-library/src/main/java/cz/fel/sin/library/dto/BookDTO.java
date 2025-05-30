package cz.fel.sin.library.dto;

import cz.fel.sin.library.model.*;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
public class BookDTO extends AbstractDTO {

    private Integer id;

    private String ISBN;

    private String name;

    private LocalDate published;

    private Publisher publisher;

    private Genre genre;

    private List<Author> author;

    public BookDTO() {
    }

    @Override
    public BookDTO clone() throws CloneNotSupportedException {
        super.clone();
        return new BookDTO();
    }
}

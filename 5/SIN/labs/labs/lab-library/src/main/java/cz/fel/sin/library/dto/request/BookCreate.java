package cz.fel.sin.library.dto.request;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode
public class BookCreate {

    public BookCreate() {
    }

    @NotEmpty
    @Size(max = 2000)
    private String name;
    @NotEmpty
    private String isbn;

    private LocalDate published;

    private Integer publisher;
    @NotNull
    private Integer genre;
    @NotEmpty
    private List<Integer> authors;
}

package cvut.fel.dto.request;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@Builder
public class BookCreate {

    @NotEmpty
    @Size(max = 2000)
    private String name;
    @NotEmpty
    private String isbn;
    @NotNull
    private Long genre;
    @NotEmpty
    private List<Long> authors;

}

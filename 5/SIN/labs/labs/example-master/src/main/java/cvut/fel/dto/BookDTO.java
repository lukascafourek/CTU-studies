package cvut.fel.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.List;

@Getter
@Setter
public class BookDTO extends AbstractDTO{

    private String ISBN;
    private Date published;


    public BookDTO() {
    }

    public BookDTO(BookDTO dto) {
        super(dto);
        ISBN = dto.getISBN();
        published = (Date) dto.getPublished().clone();
    }

    public Date getPublished() {
        return (Date) published.clone();
    }

    public void setPublished(Date published) {
        this.published = (Date) published.clone();
    }

    @Override
    public AbstractDTO clone() {
        return new BookDTO(this);
    }

}

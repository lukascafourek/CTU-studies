package sin.library.libraryapp.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class PublisherDto {

    Integer id;
    String name;
    Integer addressId;
    List<Integer> booksIds;
    List<Integer> contractsIds;

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

    public List<Integer> getContractsIds() {
        if (contractsIds != null) {
            return new ArrayList<>(contractsIds);
        }
        return null;
    }

    public void setContractsIds(List<Integer> contractsIds) {
        if (contractsIds != null) {
            this.contractsIds = new ArrayList<>(contractsIds);
        }
    }
}

package sin.library.libraryapp.dto;

import lombok.Data;

@Data
public class AddressDto {

    private Integer id;
    private String street;
    private Integer houseNumber;
    private Integer cityId;
}

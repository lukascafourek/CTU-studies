package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.AddressDto;
import sin.library.libraryapp.model.Address;

@Mapper(componentModel = "spring")
public interface AddressMapper {

    @Mapping(target = "cityId", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "street", target = "street")
    @Mapping(source = "houseNumber", target = "houseNumber")
    AddressDto toDto(Address address);

    @Mapping(target = "city", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "street", target = "street")
    @Mapping(source = "houseNumber", target = "houseNumber")
    Address toEntity(AddressDto addressDto);
}

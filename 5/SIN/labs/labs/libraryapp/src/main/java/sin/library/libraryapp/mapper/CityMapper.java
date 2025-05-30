package sin.library.libraryapp.mapper;

import org.mapstruct.*;
import sin.library.libraryapp.dto.CityDto;
import sin.library.libraryapp.model.City;

@Mapper(componentModel = "spring")
public interface CityMapper {
    @Mapping(target = "addresses", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    City toEntity(CityDto cityDto);

    @Mapping(target = "addressesIds", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    CityDto toDto(City city);
}

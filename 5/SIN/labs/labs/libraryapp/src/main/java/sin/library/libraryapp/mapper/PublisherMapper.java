package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.PublisherDto;
import sin.library.libraryapp.model.Publisher;

@Mapper(componentModel = "spring")
public interface PublisherMapper {

    @Mapping(target = "address", ignore = true)
    @Mapping(target = "books", ignore = true)
    @Mapping(target = "contracts", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    Publisher toEntity(PublisherDto publisherDto);

    @Mapping(target = "addressId", ignore = true)
    @Mapping(target = "booksIds", ignore = true)
    @Mapping(target = "contractsIds", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    PublisherDto toDto(Publisher publisher);
}

package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.LibraryDto;
import sin.library.libraryapp.model.Library;

@Mapper(componentModel = "spring")
public interface LibraryMapper {

    @Mapping(target = "books", ignore = true)
    @Mapping(target = "address", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    Library toEntity(LibraryDto libraryDto);

    @Mapping(target = "bookIds", ignore = true)
    @Mapping(target = "addressId", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    LibraryDto toDto(Library library);
}

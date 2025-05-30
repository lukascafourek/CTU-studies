package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.AuthorDto;
import sin.library.libraryapp.model.Author;

@Mapper(componentModel = "spring")
public interface AuthorMapper {

    @Mapping(target = "address", ignore = true)
    @Mapping(target = "books", ignore = true)
    @Mapping(target = "publishers", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "firstName", target = "firstName")
    @Mapping(source = "surName", target = "surName")
    @Mapping(source = "email", target = "email")
    Author toEntity(AuthorDto authorDto);

    @Mapping(target = "addressId", ignore = true)
    @Mapping(target = "booksIds", ignore = true)
    @Mapping(target = "publishersIds", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "firstName", target = "firstName")
    @Mapping(source = "surName", target = "surName")
    @Mapping(source = "email", target = "email")
    AuthorDto toDto(Author author);
}

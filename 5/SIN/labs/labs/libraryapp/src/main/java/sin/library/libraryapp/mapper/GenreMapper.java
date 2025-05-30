package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.GenreDto;
import sin.library.libraryapp.model.Genre;

@Mapper(componentModel = "spring")
public interface GenreMapper {

    @Mapping(target = "bookIds", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    GenreDto toDto(Genre genre);

    @Mapping(target = "books", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "name", target = "name")
    Genre toEntity(GenreDto genreDto);
}

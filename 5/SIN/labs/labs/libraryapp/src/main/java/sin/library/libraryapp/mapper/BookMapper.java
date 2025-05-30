package sin.library.libraryapp.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import sin.library.libraryapp.dto.BookDto;
import sin.library.libraryapp.model.Book;

@Mapper(componentModel = "spring")
public interface BookMapper {

    @Mapping(target = "publisher", ignore = true)
    @Mapping(target = "genre", ignore = true)
    @Mapping(target = "library", ignore = true)
    @Mapping(target = "authors", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "isbn", target = "isbn")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "published", target = "published")
    Book toEntity(BookDto bookDto);

    @Mapping(target = "publisherId", ignore = true)
    @Mapping(target = "genreId", ignore = true)
    @Mapping(target = "libraryId", ignore = true)
    @Mapping(target = "authorsIds", ignore = true)
    @Mapping(source = "id", target = "id")
    @Mapping(source = "isbn", target = "isbn")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "published", target = "published")
    BookDto toDto(Book book);
}

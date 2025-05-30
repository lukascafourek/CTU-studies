package cz.fel.sin.library.dto;

import cz.fel.sin.library.dto.request.BookCreate;
import cz.fel.sin.library.model.Book;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface RequestMapper {

    @Mapping(target = "publisher", ignore = true)
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "genre", ignore = true)
    @Mapping(target = "author", ignore = true)
    @Mapping(source = "name", target = "name")
    @Mapping(source = "published", target = "published")
    @Mapping(source = "isbn", target = "ISBN")
    Book createToBook(BookCreate bookCreate);
}

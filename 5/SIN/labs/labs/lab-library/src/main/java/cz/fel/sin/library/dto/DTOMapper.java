package cz.fel.sin.library.dto;

import cz.fel.sin.library.model.*;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface DTOMapper {

    BookDTO bookToDto(Book book);
}

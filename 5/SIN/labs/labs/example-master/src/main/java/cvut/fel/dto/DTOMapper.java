package cvut.fel.dto;

import cvut.fel.entity.*;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface DTOMapper {
    BookDTO bookToDto(Book book);

}

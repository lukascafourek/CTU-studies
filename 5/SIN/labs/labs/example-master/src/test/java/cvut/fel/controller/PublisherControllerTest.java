package cvut.fel.controller;

import cvut.fel.dto.DTOMapper;
import cvut.fel.dto.request.BookCreate;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.SpyBean;

@ExtendWith(MockitoExtension.class)
public class PublisherControllerTest extends BaseControllerTestRunner {

    @SpyBean
    private DTOMapper dtoMapper;


    private BookCreate generateBookCreate() {
        return BookCreate.builder()
                .name("Nova Kniha")
                .genre(1L)
                .isbn("1236")
                .build();
    }

}

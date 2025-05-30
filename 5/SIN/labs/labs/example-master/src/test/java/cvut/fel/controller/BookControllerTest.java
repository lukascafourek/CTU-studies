package cvut.fel.controller;

import cvut.fel.dto.BookDTO;
import cvut.fel.dto.DTOMapper;
import cvut.fel.entity.Book;
import cvut.fel.service.BookService;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.web.servlet.MvcResult;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

@ExtendWith(MockitoExtension.class)
public class BookControllerTest extends BaseControllerTestRunner {

    @MockBean
    private BookService bookServiceMock;

    @SpyBean
    private DTOMapper dtoMapper;

    @Autowired
    private BookController sut;

    @Test
    public void getByIdReturnsMatchingBook() throws Exception {
        final Book book = new Book("name");
        book.setId(new Long(1));
        when(bookServiceMock.findById(book.getId())).thenReturn(book);
        final MvcResult mvcResult = mockMvc.perform(get("/book/" + book.getId())).andReturn();

        final BookDTO result = readValue(mvcResult, BookDTO.class);
        verify(dtoMapper).bookToDto(book);
        assertNotNull(result);
        assertEquals(book.getId(), result.getId());
        assertEquals(book.getName(), result.getName());
    }
}

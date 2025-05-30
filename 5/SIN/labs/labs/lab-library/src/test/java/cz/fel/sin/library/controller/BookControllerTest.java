package cz.fel.sin.library.controller;

import cz.fel.sin.library.dto.BookDTO;
import cz.fel.sin.library.dto.DTOMapper;
import cz.fel.sin.library.exception.NotFoundException;
import cz.fel.sin.library.model.Book;
import cz.fel.sin.library.service.BookService;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.web.servlet.MvcResult;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
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
    public void getBookByIdReturnsMatchingBook() throws Exception {
        final Book book = new Book();
        book.setName("name");
        book.setId(1);
        when(bookServiceMock.findById(book.getId())).thenReturn(book);
        final MvcResult mvcResult = mockMvc.perform(get("/book/" + book.getId())).andReturn();

        final BookDTO result = readValue(mvcResult, BookDTO.class);
        verify(dtoMapper).bookToDto(book);
        assertNotNull(result);
        assertEquals(book.getId(), result.getId());
        assertEquals(book.getName(), result.getName());
    }

    @Test
    public void getBookByIdReturnsNotFoundWhenBookDoesNotExist() throws Exception {
        when(bookServiceMock.findById(1)).thenThrow(new NotFoundException("BOOK_NOT_FOUND"));
        final MvcResult mvcResult = mockMvc.perform(get("/book/1")).andReturn();

        assertEquals(404, mvcResult.getResponse().getStatus());
    }

    @Test
    public void getBookByIdReturnsBadRequestWhenIdIsInvalid() throws Exception {
        final MvcResult mvcResult = mockMvc.perform(get("/book/invalid")).andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
    }
}

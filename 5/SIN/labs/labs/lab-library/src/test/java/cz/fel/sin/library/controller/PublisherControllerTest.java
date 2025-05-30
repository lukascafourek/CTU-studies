package cz.fel.sin.library.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import cz.fel.sin.library.dto.request.BookCreate;
import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.service.PublisherService;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.LocalDate;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;

@ExtendWith(MockitoExtension.class)
public class PublisherControllerTest extends BaseControllerTestRunner {

    @MockBean
    private PublisherService publisherService;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private PublisherController sut;

    private String asJsonString(final Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private BookCreate generateBookCreate() {
        BookCreate bookCreate = new BookCreate();
        bookCreate.setName("Book");
        bookCreate.setIsbn("1234567890");
        bookCreate.setGenre(1);
        bookCreate.setPublished(LocalDate.now());
        bookCreate.setAuthors(List.of(1, 2));
        return bookCreate;
    }

    @Test
    public void publishNewBookReturnsSuccessWhenBookPublished() throws Exception {
        BookCreate bookCreate = generateBookCreate();
        when(publisherService.publishNewBook(1, bookCreate)).thenReturn(1);
        final MvcResult mvcResult = mockMvc.perform(post("/publisher/1/book")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(bookCreate)))
                .andReturn();

        assertEquals(200, mvcResult.getResponse().getStatus());
        assertEquals("Book published successfully.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void publishNewBookReturnsBadRequestWhenBookNotPublished() throws Exception {
        BookCreate bookCreate = generateBookCreate();
        when(publisherService.publishNewBook(1, bookCreate)).thenReturn(0);
        final MvcResult mvcResult = mockMvc.perform(post("/publisher/1/book")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(bookCreate)))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Failed to publish the book.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void publishNewBookReturnsBadRequestWhenFieldInvalidExceptionThrown() throws Exception {
        BookCreate bookCreate = generateBookCreate();
        when(publisherService.publishNewBook(1, bookCreate)).thenThrow(new FieldInvalidException("Invalid field"));
        final MvcResult mvcResult = mockMvc.perform(post("/publisher/1/book")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(bookCreate)))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Invalid field", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void createContractReturnsSuccessWhenContractCreated() throws Exception {
        when(publisherService.createContract(1, 1)).thenReturn(true);
        final MvcResult mvcResult = mockMvc.perform(patch("/publisher/1/author/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(200, mvcResult.getResponse().getStatus());
        assertEquals("Contract created successfully.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void createContractReturnsBadRequestWhenContractNotCreated() throws Exception {
        when(publisherService.createContract(1, 1)).thenReturn(false);
        final MvcResult mvcResult = mockMvc.perform(patch("/publisher/1/author/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Failed to create the contract.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void createContractReturnsBadRequestWhenFieldInvalidExceptionThrown() throws Exception {
        when(publisherService.createContract(1, 1)).thenThrow(new FieldInvalidException("Invalid field"));
        final MvcResult mvcResult = mockMvc.perform(patch("/publisher/1/author/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Invalid field", mvcResult.getResponse().getContentAsString());
    }
}

package cz.fel.sin.library.controller;

import cz.fel.sin.library.exception.FieldInvalidException;
import cz.fel.sin.library.model.Library;
import cz.fel.sin.library.service.LibraryService;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;

@ExtendWith(MockitoExtension.class)
public class LibraryControllerTest extends BaseControllerTestRunner {

    @MockBean
    private LibraryService libraryServiceMock;

    @Autowired
    private LibraryController sut;

    @Test
    public void getLibraryByIdReturnsLibrary() throws Exception {
        Library library = new Library();
        library.setId(1);
        when(libraryServiceMock.findById(1)).thenReturn(library);
        final MvcResult mvcResult = mockMvc.perform(get("/library/" + library.getId())).andReturn();

        assertEquals(200, mvcResult.getResponse().getStatus());
    }

    @Test
    public void addBookToLibraryReturnsSuccessWhenBookAdded() throws Exception {
        when(libraryServiceMock.addBookToLibrary(1, 1)).thenReturn(true);
        final MvcResult mvcResult = mockMvc.perform(patch("/library/1/book/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(200, mvcResult.getResponse().getStatus());
        assertEquals("Book added to library successfully.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void addBookToLibraryReturnsBadRequestWhenBookNotAdded() throws Exception {
        when(libraryServiceMock.addBookToLibrary(1, 1)).thenReturn(false);
        final MvcResult mvcResult = mockMvc.perform(patch("/library/1/book/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Failed to add book to library.", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void addBookToLibraryReturnsBadRequestWhenFieldInvalidExceptionThrown() throws Exception {
        when(libraryServiceMock.addBookToLibrary(1, 1)).thenThrow(new FieldInvalidException("Invalid field"));
        final MvcResult mvcResult = mockMvc.perform(patch("/library/1/book/1")
                        .contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        assertEquals(400, mvcResult.getResponse().getStatus());
        assertEquals("Invalid field", mvcResult.getResponse().getContentAsString());
    }
}

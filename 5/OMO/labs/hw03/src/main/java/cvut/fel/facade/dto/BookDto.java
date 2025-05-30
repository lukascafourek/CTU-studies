package cvut.fel.facade.dto;

import org.springframework.context.annotation.EnableMBeanExport;

public class BookDto {
    private String bookName;
    private String firstAuthorFirstName;
    private String NameOfFirstLibrary;

    public String getBookName() {
        return bookName;
    }

    public String getFirstAuthorFirstName() {
        return firstAuthorFirstName;
    }

    public String getNameOfFirstLibrary() {
        return NameOfFirstLibrary;
    }

    public BookDto(String bookName, String firstAuthorFirstName, String nameOfFirstLibrary) {
        this.bookName = bookName;
        this.firstAuthorFirstName = firstAuthorFirstName;
        NameOfFirstLibrary = nameOfFirstLibrary;
    }
}

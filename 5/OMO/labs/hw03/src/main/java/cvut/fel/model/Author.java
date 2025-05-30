package cvut.fel.model;

public class Author {

    private int id;

    private String firstname;

    private String surname;

    private Address address;

    private int bookId;

    public Author(int id, String firstname, String surname, Address address, int bookId) {
        this.id = id;
        this.firstname = firstname;
        this.surname = surname;
        this.address = address;
        this.bookId = bookId;
    }

    public Author() {
    }

    public int getBook() {
        return bookId;
    }

    public void setBook(int book) {
        this.bookId = book;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getFirstname() {
        return firstname;
    }

    public void setFirstname(String firstname) {
        this.firstname = firstname;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public Address getAddress() {
        return address;
    }

    public void setAddress(Address address) {
        this.address = address;
    }
}

package cvut.fel.model;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class Book {
    private int id;
    private Long ISBN;

    private Long publishingHouseId;

    private String name;

    private int ownerId;

    private int libraryId;

    private String type;

    public Book() {
    }

    public Book(int id, Long isbn, String name) {
        this.id = id;
        this.ISBN = isbn;
        this.name = name;
    }

    public int getId() {
        return id;
    }
    public void incrementId() {
        //This is thread safe (different threads will always receive distinct values, and no values will be "lost")
        id = new AtomicInteger().getAndIncrement();
    }
    public void setId(int id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "Book{" +
                "id=" + ISBN +
                ", name='" + name + '\'' +
                '}';
    }

    public Long getISBN() {
        return ISBN;
    }

    public void setISBN(Long ISBN) {
        this.ISBN = ISBN;
    }

    public Long getPublishingHouseId() {
        return publishingHouseId;
    }

    public void setPublishingHouseId(Long publishingHouseId) {
        this.publishingHouseId = publishingHouseId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getOwnerId() {
        return ownerId;
    }

    public void setOwnerId(int ownerId) {
        this.ownerId = ownerId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getLibraryId() {
        return libraryId;
    }

    public void setLibraryId(int libraryId) {
        this.libraryId = libraryId;
    }
}

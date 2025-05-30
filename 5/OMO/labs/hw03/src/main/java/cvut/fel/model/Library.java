package cvut.fel.model;

import java.util.ArrayList;
import java.util.List;

public class Library {

    private int id;

    private String name;

    private Address address;


    public void setId(int id) {
        this.id = id;
    }

    public List<Integer> getBooksIds() {
        return booksIds;
    }

    public void setBooksIds(List<Integer> booksIds) {
        this.booksIds = booksIds;
    }

    List<Integer> booksIds;
    public Library(int id, String name, Address address, List<Integer> booksIds)  {
        this.id = id;
        this.name = name;
        this.address = address;
        this.booksIds = booksIds;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Address getAddress() {
        return address;
    }

    public void setAddress(Address address) {
        this.address = address;
    }

    public int getId() {
        return id;
    }

    @Override
    public String toString() {
        return "Library{" +
                "id=" + id +
                ", name='" + name + '\'' +
                '}';
    }
}

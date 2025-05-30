package cz.fel.sin.library.model;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Entity
@Table(name = "author")
@Getter
@Setter
public class Author {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "sur_name")
    private String surName;

    @Enumerated(EnumType.STRING)
    private Gender gender;

    @OneToOne
    @JoinColumn(name = "address_id")
    private Address address;

    @ManyToMany(mappedBy = "contracts")
    private List<Publisher> publishers;

    @ManyToMany(mappedBy = "author")
    private List<Book> books;

    @Override
    public String toString() {
        return "Author{" +
                "id=" + id +
                ", firstName='" + firstName + '\'' +
                ", surName='" + surName + '\'' +
                ", gender=" + gender +
                ", address=" + address +
                ", publishers=" + publishers +
                ", books=" + books +
                '}';
    }
}

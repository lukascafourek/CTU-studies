package cz.fel.sin.library.model;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "address")
@Getter
@Setter
public class Address {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(name = "post_code")
    private Integer postCode;

    @Column
    private String street;

    @Column(name = "house_number")
    private Integer houseNumber;

    @Override
    public String toString() {
        return "Address{" +
                "id=" + id +
                ", postCode=" + postCode +
                ", street='" + street + '\'' +
                ", houseNumber=" + houseNumber +
                '}';
    }
}

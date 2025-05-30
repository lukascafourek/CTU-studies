package cz.cvut.fel.sin.sintest.model;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Entity
@Table(name = "group")
@Getter
@Setter
public class Group {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Enumerated(EnumType.STRING)
    private Wisdom groupWisdom;

    @OneToMany(mappedBy = "group")
    private List<Hero> heroes;

    @OneToMany(mappedBy = "goodGroup")
    private List<Fight> goodFights;

    @OneToMany(mappedBy = "evilGroup")
    private List<Fight> evilFights;

    @OneToMany(mappedBy = "winnerGroup")
    private List<Fight> winnerFights;

    @Override
    public String toString() {
        return "Group{" +
                "id=" + id +
                ", groupWisdom=" + groupWisdom +
                ", heroes=" + heroes +
                ", goodFights=" + goodFights +
                ", evilFights=" + evilFights +
                ", winnerFights=" + winnerFights +
                '}';
    }
}

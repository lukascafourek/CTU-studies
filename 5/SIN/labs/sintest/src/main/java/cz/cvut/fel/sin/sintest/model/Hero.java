package cz.cvut.fel.sin.sintest.model;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "hero")
@Getter
@Setter
public class Hero {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(unique = true, name = "name")
    private String name;

    @Enumerated(EnumType.STRING)
    private Wisdom wisdom;

    @Column(name = "power")
    private Integer power;

    @Column(name = "will")
    private Integer will;

    @Column(name = "mana")
    private Integer mana;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id")
    private Group group;

    @Override
    public String toString() {
        return "Hero{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", wisdom=" + wisdom +
                ", power=" + power +
                ", will=" + will +
                ", mana=" + mana +
                ", group=" + group +
                '}';
    }
}

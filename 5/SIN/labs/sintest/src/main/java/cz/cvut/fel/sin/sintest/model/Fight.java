package cz.cvut.fel.sin.sintest.model;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "fight")
@Getter
@Setter
public class Fight {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Enumerated(EnumType.STRING)
    private TypeOfFight typeOfFight;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "good_group_id")
    private Group goodGroup;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "evil_group_id")
    private Group evilGroup;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "winner_group_id")
    private Group winnerGroup;

    @Override
    public String toString() {
        return "Fight{" +
                "id=" + id +
                ", typeOfFight=" + typeOfFight +
                ", goodGroup=" + goodGroup +
                ", evilGroup=" + evilGroup +
                ", winnerGroup=" + winnerGroup +
                '}';
    }
}

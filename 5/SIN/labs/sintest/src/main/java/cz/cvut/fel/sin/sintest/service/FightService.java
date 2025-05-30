package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.model.Fight;
import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.Hero;
import cz.cvut.fel.sin.sintest.model.TypeOfFight;

import java.util.function.ToIntFunction;

public interface FightService {

    Group fight(Integer group1Id, Integer group2Id, TypeOfFight typeOfFight);

    boolean validateGroupWisdom(Group group1, Group group2);

    Group fightByAttribute(Group group1, Group group2, ToIntFunction<Hero> attribute);

    void addFightToGroups(Group group1, Group group2, Fight fight);
}

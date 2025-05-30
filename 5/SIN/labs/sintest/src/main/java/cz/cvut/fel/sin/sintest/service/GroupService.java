package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.Hero;

public interface GroupService {

    boolean addHeroToGroup(Integer groupId, Integer heroId);

    boolean validateHeroName(Group group, Hero hero);

    boolean validateWisdom(Group group, Hero hero);
}

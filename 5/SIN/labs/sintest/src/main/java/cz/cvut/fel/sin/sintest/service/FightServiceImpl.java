package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.FightCannotHappenException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.model.*;
import cz.cvut.fel.sin.sintest.repository.FightRepository;
import cz.cvut.fel.sin.sintest.repository.GroupRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.function.ToIntFunction;

@Service
@RequiredArgsConstructor
@Log4j2
public class FightServiceImpl implements FightService {

    private final FightRepository fightRepository;

    private final GroupRepository groupRepository;

    @Override
    public Group fight(Integer group1Id, Integer group2Id, TypeOfFight typeOfFight) {
        log.info("fight entered with group1Id: {}, group2Id: {}", group1Id, group2Id);
        if (group1Id == null)
            throw new FieldMissingException("GROUP1_ID_IS_NULL");
        if (group2Id == null)
            throw new FieldMissingException("GROUP2_ID_IS_NULL");
        Group group1 = groupRepository.findById(group1Id)
                .orElseThrow(() -> new NotFoundException("GROUP1_NOT_FOUND"));
        Group group2 = groupRepository.findById(group2Id)
                .orElseThrow(() -> new NotFoundException("GROUP2_NOT_FOUND"));
        if (validateGroupWisdom(group1, group2)) {
            throw new FightCannotHappenException("GROUPS_HAVE_SAME_WISDOM");
        }
        Fight fight = new Fight();
        fight.setGoodGroup(group1.getGroupWisdom().equals(Wisdom.GOOD) ? group1 : group2);
        fight.setEvilGroup(group1.getGroupWisdom().equals(Wisdom.EVIL) ? group1 : group2);
        fight.setTypeOfFight(typeOfFight);
        Group winner = switch (typeOfFight) {
            case POWER -> fightByAttribute(group1, group2, Hero::getPower);
            case WILL -> fightByAttribute(group1, group2, Hero::getWill);
            case MANA -> fightByAttribute(group1, group2, Hero::getMana);
        };
        fight.setWinnerGroup(winner);
        fightRepository.save(fight);
        addFightToGroups(group1, group2, fight);
        if (winner.equals(group1)) {
            group1.getWinnerFights().add(fight);
        } else {
            group2.getWinnerFights().add(fight);
        }
        groupRepository.save(group1);
        groupRepository.save(group2);
        log.info("fight exited with winner: {}", winner);
        return winner;
    }

    @Override
    public boolean validateGroupWisdom(Group group1, Group group2) {
        return group1.getGroupWisdom().equals(group2.getGroupWisdom());
    }

    @Override
    public Group fightByAttribute(Group group1, Group group2, ToIntFunction<Hero> attributeGetter) {
        int group1Attribute = group1.getHeroes().stream().mapToInt(attributeGetter).sum();
        int group2Attribute = group2.getHeroes().stream().mapToInt(attributeGetter).sum();
        return group1Attribute > group2Attribute ? group1 : group2;
    }

    @Override
    public void addFightToGroups(Group group1, Group group2, Fight fight) {
        if (group1.getGroupWisdom().equals(Wisdom.GOOD)) {
            group1.getGoodFights().add(fight);
            group2.getEvilFights().add(fight);
        } else {
            group1.getEvilFights().add(fight);
            group2.getGoodFights().add(fight);
        }
    }
}

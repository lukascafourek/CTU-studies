package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.exception.AlreadyExistsException;
import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.exception.ValueNotSameException;
import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.Hero;
import cz.cvut.fel.sin.sintest.repository.GroupRepository;
import cz.cvut.fel.sin.sintest.repository.HeroRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class GroupServiceImpl implements GroupService {

    private final GroupRepository groupRepository;

    private final HeroRepository heroRepository;

    public boolean addHeroToGroup(Integer groupId, Integer heroId) {
        log.info("addHeroToGroup entered with groupId: {}, heroId: {}", groupId, heroId);
        if (groupId == null)
            throw new FieldMissingException("GROUP_ID_IS_NULL");
        if (heroId == null)
            throw new FieldMissingException("HERO_ID_IS_NULL");
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new NotFoundException("GROUP_NOT_FOUND"));
        Hero hero = heroRepository.findById(heroId)
                .orElseThrow(() -> new NotFoundException("HERO_NOT_FOUND"));
        if (validateHeroName(group, hero)) {
            throw new AlreadyExistsException("HERO_ALREADY_EXISTS_IN_GROUP");
        }
        if (!validateWisdom(group, hero)) {
            throw new ValueNotSameException("HERO_WISDOM_NOT_MATCH_GROUP_WISDOM");
        }
        group.getHeroes().add(hero);
        hero.setGroup(group);
        groupRepository.save(group);
        heroRepository.save(hero);
        log.info("addHeroToGroup exited with groupId: {}, heroId: {}, return: true", groupId, heroId);
        return true;
    }

    @Override
    public boolean validateHeroName(Group group, Hero hero) {
        return group.getHeroes().stream()
                .anyMatch(h -> h.getName().equals(hero.getName()));
    }

    public boolean validateWisdom(Group group, Hero hero) {
        return group.getGroupWisdom().equals(hero.getWisdom());
    }
}

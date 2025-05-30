package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.exception.AlreadyExistsException;
import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.exception.ValueNotSameException;
import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.Hero;
import cz.cvut.fel.sin.sintest.model.Wisdom;
import cz.cvut.fel.sin.sintest.repository.GroupRepository;
import cz.cvut.fel.sin.sintest.repository.HeroRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
@ExtendWith(MockitoExtension.class)
class GroupServiceImplTest {

    @MockBean
    private GroupRepository groupRepository;

    @MockBean
    private HeroRepository heroRepository;

    @SpyBean
    private GroupServiceImpl groupService;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void addHeroToGroup_success() {
        Group group = new Group();
        group.setId(1);
        group.setGroupWisdom(Wisdom.GOOD);
        group.setHeroes(new ArrayList<>());
        Hero hero = new Hero();
        hero.setId(1);
        hero.setName("Hero1");
        hero.setWisdom(Wisdom.GOOD);

        when(groupRepository.findById(1)).thenReturn(Optional.of(group));
        when(heroRepository.findById(1)).thenReturn(Optional.of(hero));
        when(groupRepository.save(any(Group.class))).thenReturn(group);
        when(heroRepository.save(any(Hero.class))).thenReturn(hero);

        boolean result = groupService.addHeroToGroup(1, 1);

        assertTrue(result);
        verify(groupRepository).save(group);
        verify(heroRepository).save(hero);
    }

    @Test
    void addHeroToGroup_groupIdIsNull_throwsFieldMissingException() {
        FieldMissingException exception = assertThrows(FieldMissingException.class, () -> groupService.addHeroToGroup(null, 1));
        assertEquals("GROUP_ID_IS_NULL", exception.getMessage());
    }

    @Test
    void addHeroToGroup_heroIdIsNull_throwsFieldMissingException() {
        FieldMissingException exception = assertThrows(FieldMissingException.class, () -> groupService.addHeroToGroup(1, null));
        assertEquals("HERO_ID_IS_NULL", exception.getMessage());
    }

    @Test
    void addHeroToGroup_groupNotFound_throwsNotFoundException() {
        when(groupRepository.findById(1)).thenReturn(Optional.empty());

        NotFoundException exception = assertThrows(NotFoundException.class, () -> groupService.addHeroToGroup(1, 1));
        assertEquals("GROUP_NOT_FOUND", exception.getMessage());
    }

    @Test
    void addHeroToGroup_heroNotFound_throwsNotFoundException() {
        Group group = new Group();
        group.setId(1);
        group.setHeroes(new ArrayList<>());

        when(groupRepository.findById(1)).thenReturn(Optional.of(group));
        when(heroRepository.findById(1)).thenReturn(Optional.empty());

        NotFoundException exception = assertThrows(NotFoundException.class, () -> groupService.addHeroToGroup(1, 1));
        assertEquals("HERO_NOT_FOUND", exception.getMessage());
    }

    @Test
    void addHeroToGroup_heroAlreadyExistsInGroup_throwsAlreadyExistsException() {
        Group group = new Group();
        group.setId(1);
        group.setHeroes(new ArrayList<>());
        Hero hero = new Hero();
        hero.setId(1);
        hero.setName("Hero1");
        group.getHeroes().add(hero);

        when(groupRepository.findById(1)).thenReturn(Optional.of(group));
        when(heroRepository.findById(1)).thenReturn(Optional.of(hero));

        AlreadyExistsException exception = assertThrows(AlreadyExistsException.class, () -> groupService.addHeroToGroup(1, 1));
        assertEquals("HERO_ALREADY_EXISTS_IN_GROUP", exception.getMessage());
    }

    @Test
    void addHeroToGroup_heroWisdomNotMatchGroupWisdom_throwsValueNotSameException() {
        Group group = new Group();
        group.setId(1);
        group.setGroupWisdom(Wisdom.GOOD);
        group.setHeroes(new ArrayList<>());
        Hero hero = new Hero();
        hero.setId(1);
        hero.setName("Hero1");
        hero.setWisdom(Wisdom.EVIL);

        when(groupRepository.findById(1)).thenReturn(Optional.of(group));
        when(heroRepository.findById(1)).thenReturn(Optional.of(hero));

        ValueNotSameException exception = assertThrows(ValueNotSameException.class, () -> groupService.addHeroToGroup(1, 1));
        assertEquals("HERO_WISDOM_NOT_MATCH_GROUP_WISDOM", exception.getMessage());
    }
}

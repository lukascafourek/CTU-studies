package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.model.Hero;

import java.util.List;

public interface HeroService {

    Hero getById(Integer id);

    List<Hero> getAll();

    boolean addHero(String name, Integer power, Integer will, Integer mana);

    boolean updateHero(Integer id, String name, Integer power, Integer will, Integer mana);
}

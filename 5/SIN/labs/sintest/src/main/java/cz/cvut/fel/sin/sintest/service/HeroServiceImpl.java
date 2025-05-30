package cz.cvut.fel.sin.sintest.service;

import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.model.Hero;
import cz.cvut.fel.sin.sintest.repository.HeroRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import springfox.documentation.annotations.Cacheable;

import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class HeroServiceImpl implements HeroService {

    private final HeroRepository heroRepository;

    @Cacheable(value = "cacheA")
    @Override
    public Hero getById(Integer id) {
        log.info("getById entered with id: {}", id);
        if (id == null)
            throw new FieldMissingException("ID_IS_NULL");
        log.info("getById exited with id: {}", id);
        return heroRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("HERO_NOT_FOUND"));
    }

    @Cacheable(value = "cacheB")
    @Override
    public List<Hero> getAll() {
        log.info("getAll entered");
        List<Hero> heroes = heroRepository.findAll();
        if (heroes.isEmpty())
            throw new NotFoundException("NO_HEROES_FOUND");
        log.info("getAll exited with {} heroes", heroes.size());
        return heroes;
    }

    @Override
    public boolean addHero(String name, Integer power, Integer will, Integer mana) {
        log.info("addHero entered with name: {}", name);
        Hero hero = new Hero();
        hero.setName(name);
        hero.setPower(power);
        hero.setWill(will);
        hero.setMana(mana);
        heroRepository.save(hero);
        log.info("addHero exited with hero: {}", hero);
        return true;
    }

    @Override
    public boolean updateHero(Integer id, String name, Integer power, Integer will, Integer mana) {
        log.info("addHero entered with id {}, name: {}", id, name);
        Hero hero = heroRepository.findById(id)
                .orElseThrow(() -> new NotFoundException("HERO_NOT_FOUND"));
        hero.setName(name);
        hero.setPower(power);
        hero.setWill(will);
        hero.setMana(mana);
        heroRepository.save(hero);
        log.info("updateHero exited with hero: {}", hero);
        return true;
    }
}

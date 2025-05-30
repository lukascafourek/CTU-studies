package cz.cvut.fel.sin.sintest.repository;

import cz.cvut.fel.sin.sintest.model.Hero;
import org.springframework.data.jpa.repository.JpaRepository;

public interface HeroRepository extends JpaRepository<Hero, Integer> {
}
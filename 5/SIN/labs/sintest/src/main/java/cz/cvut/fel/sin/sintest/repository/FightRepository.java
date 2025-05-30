package cz.cvut.fel.sin.sintest.repository;

import cz.cvut.fel.sin.sintest.model.Fight;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FightRepository extends JpaRepository<Fight, Integer> {
}
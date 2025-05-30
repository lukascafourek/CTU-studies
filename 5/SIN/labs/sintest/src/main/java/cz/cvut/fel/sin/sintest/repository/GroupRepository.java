package cz.cvut.fel.sin.sintest.repository;

import cz.cvut.fel.sin.sintest.model.Group;
import org.springframework.data.jpa.repository.JpaRepository;

public interface GroupRepository extends JpaRepository<Group, Integer> {
}
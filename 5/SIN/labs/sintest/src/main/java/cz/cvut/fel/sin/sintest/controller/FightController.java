package cz.cvut.fel.sin.sintest.controller;

import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.FightCannotHappenException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.TypeOfFight;
import cz.cvut.fel.sin.sintest.service.FightService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController("/fight")
@RequiredArgsConstructor
public class FightController {

    private final FightService fightService;

    @PostMapping("/fight")
    public ResponseEntity<String> fight(@RequestParam Integer groupId1,
                                        @RequestParam Integer groupId2,
                                        @RequestParam TypeOfFight typeOfFight) {
        try {
            Group winner = fightService.fight(groupId1, groupId2, typeOfFight);
            if (winner != null) {
                return ResponseEntity.ok("Fight successful. Winner is " + winner.getId());
            } else {
                return ResponseEntity.badRequest().body("Failed to fight.");
            }
        } catch (FieldMissingException | NotFoundException | FightCannotHappenException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
}

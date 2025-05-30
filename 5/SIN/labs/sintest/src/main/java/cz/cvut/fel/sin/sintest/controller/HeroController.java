package cz.cvut.fel.sin.sintest.controller;

import cz.cvut.fel.sin.sintest.exception.FieldInvalidException;
import cz.cvut.fel.sin.sintest.model.Hero;
import cz.cvut.fel.sin.sintest.service.HeroService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController("/hero")
@RequiredArgsConstructor
public class HeroController {

    private final HeroService heroService;

    @PostMapping("/hero")
    public ResponseEntity<String> addHero(@RequestParam String name,
                                          @RequestParam Integer power,
                                          @RequestParam Integer will,
                                          @RequestParam Integer mana) {
        try {
            boolean result = heroService.addHero(name, power, will, mana);
            if (result) {
                return ResponseEntity.ok("Hero added successfully.");
            } else {
                return ResponseEntity.badRequest().body("Failed to add hero.");
            }
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @PatchMapping("/hero/{id}")
    public ResponseEntity<String> updateHero(@PathVariable("id") Integer id,
                                             @RequestParam String name,
                                             @RequestParam Integer power,
                                             @RequestParam Integer will,
                                             @RequestParam Integer mana) {
        try {
            boolean result = heroService.updateHero(id, name, power, will, mana);
            if (result) {
                return ResponseEntity.ok("Hero updated successfully.");
            } else {
                return ResponseEntity.badRequest().body("Failed to update hero.");
            }
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @GetMapping("/hero")
    public ResponseEntity<String> getAllHeroes() {
        try {
            List<Hero> result = heroService.getAll();
            return ResponseEntity.ok(result.toString());
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }

    @GetMapping("/hero/{id}")
    public ResponseEntity<String> getHeroById(@PathVariable("id") Integer id) {
        try {
            Hero result = heroService.getById(id);
            return ResponseEntity.ok(result.toString());
        } catch (FieldInvalidException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        }
    }
}

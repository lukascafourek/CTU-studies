INSERT INTO "group" (group_wisdom) VALUES ('Good');
INSERT INTO "group" (group_wisdom) VALUES ('Evil');

INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Gandalf', 2, 5, 10, 'Good', 1);
INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Bilbo', 3, 10, 1, 'Good', 1);
INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Thorin', 10, 5, 0, 'Good', 1);

INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Bolg', 10, 10, 0, 'Evil', 2);
INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Golfimbul', 6, 4, 0, 'Evil', 2);
INSERT INTO hero (name, power, will, mana, wisdom, group_id) VALUES ('Necromancer', 3, 3, 10, 'Evil', 2);
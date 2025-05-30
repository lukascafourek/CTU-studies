-- liquibase formatted sql

-- changeset lukascafourek:1736246767560-7
ALTER TABLE fight
    DROP CONSTRAINT fk_fight_on_group;

-- changeset lukascafourek:1736246767560-1
ALTER TABLE fight
    ADD evil_group_id INTEGER;
ALTER TABLE fight
    ADD good_group_id INTEGER;
ALTER TABLE fight
    ADD winner_group_id INTEGER;

-- changeset lukascafourek:1736246767560-4
ALTER TABLE fight
    ADD CONSTRAINT FK_FIGHT_ON_EVIL_GROUP FOREIGN KEY (evil_group_id) REFERENCES "group" (id);

-- changeset lukascafourek:1736246767560-5
ALTER TABLE fight
    ADD CONSTRAINT FK_FIGHT_ON_GOOD_GROUP FOREIGN KEY (good_group_id) REFERENCES "group" (id);

-- changeset lukascafourek:1736246767560-6
ALTER TABLE fight
    ADD CONSTRAINT FK_FIGHT_ON_WINNER_GROUP FOREIGN KEY (winner_group_id) REFERENCES "group" (id);

-- changeset lukascafourek:1736246767560-8
ALTER TABLE fight
    DROP COLUMN group_id;


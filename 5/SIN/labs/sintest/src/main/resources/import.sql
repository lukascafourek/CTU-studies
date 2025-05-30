-- liquibase formatted sql

-- changeset lukascafourek:1736242599796-1
CREATE TABLE fight
(
    id            INTEGER GENERATED BY DEFAULT AS IDENTITY NOT NULL,
    type_of_fight VARCHAR(255),
    group_id      INTEGER,
    CONSTRAINT pk_fight PRIMARY KEY (id)
);

-- changeset lukascafourek:1736242599796-2
CREATE TABLE "group"
(
    id            INTEGER GENERATED BY DEFAULT AS IDENTITY NOT NULL,
    group_wisdom  VARCHAR(255),
    CONSTRAINT pk_group PRIMARY KEY (id)
);

-- changeset lukascafourek:1736242599796-3
CREATE TABLE hero
(
    id       INTEGER GENERATED BY DEFAULT AS IDENTITY NOT NULL,
    name     VARCHAR(255),
    wisdom   VARCHAR(255),
    power    INTEGER,
    will     INTEGER,
    mana     INTEGER,
    group_id INTEGER,
    CONSTRAINT pk_hero PRIMARY KEY (id)
);

-- changeset lukascafourek:1736242599796-4
ALTER TABLE hero
    ADD CONSTRAINT uc_hero_name UNIQUE (name);

-- changeset lukascafourek:1736242599796-5
ALTER TABLE fight
    ADD CONSTRAINT FK_FIGHT_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (id);

-- changeset lukascafourek:1736242599796-6
ALTER TABLE hero
    ADD CONSTRAINT FK_HERO_ON_GROUP FOREIGN KEY (group_id) REFERENCES "group" (id);


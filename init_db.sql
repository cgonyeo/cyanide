BEGIN;

DROP TABLE IF EXISTS "recipes_to_glasses" CASCADE;
DROP TABLE IF EXISTS "ingredients_to_recipes" CASCADE;
DROP TABLE IF EXISTS "recipes" CASCADE;
DROP TABLE IF EXISTS "glasses" CASCADE;
DROP TABLE IF EXISTS "purchases" CASCADE;
DROP TABLE IF EXISTS "ingredients" CASCADE;
DROP TABLE IF EXISTS "ingredient_classes" CASCADE;

CREATE TABLE "ingredient_classes" (
    id   SERIAL        NOT NULL,
    name VARCHAR(1024) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE "ingredients" (
    id            SERIAL        NOT NULL,
    class         INTEGER,
    name          VARCHAR(1024) NOT NULL,
    available     BOOLEAN       NOT NULL,
    notForRecipes BOOLEAN       NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (class) REFERENCES "ingredient_classes" (id) ON DELETE SET NULL
);

CREATE TABLE "purchases" (
    ingredient INTEGER       NOT NULL,
    date       DATE          NOT NULL,
    location   VARCHAR(1024) NOT NULL,
    price      INTEGER       NOT NULL,
    volume     INTEGER       NOT NULL,
    unit       VARCHAR(1024) NOT NULL,
    FOREIGN KEY (ingredient) REFERENCES "ingredients" (id) ON DELETE CASCADE
);

CREATE TABLE "recipes" (
    id                SERIAL         NOT NULL,
    name              VARCHAR(1024),
    garnish           VARCHAR(1024)  NOT NULL,
    instructions      VARCHAR(10240) NOT NULL,
    for_ingredient_id INTEGER UNIQUE,
    PRIMARY KEY (id),
    FOREIGN KEY (for_ingredient_id) REFERENCES "ingredients" (id) ON DELETE CASCADE
);

CREATE TABLE "glasses" (
    id   SERIAL        NOT NULL,
    name VARCHAR(1024) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE "recipes_to_glasses" (
    glass_id  INTEGER NOT NULL,
    recipe_id INTEGER NOT NULL,
    FOREIGN KEY (recipe_id) REFERENCES "recipes" (id) ON DELETE CASCADE,
    FOREIGN KEY (glass_id)  REFERENCES "glasses" (id) ON DELETE CASCADE
);

CREATE TABLE "ingredients_to_recipes" (
    recipe_id          INTEGER       NOT NULL,
    ingredient_id      INTEGER,
    ingredient_class_id INTEGER,
    amount_numer       INTEGER       NOT NULL,
    amount_denom       INTEGER       NOT NULL,
    unit              VARCHAR(1024) NOT NULL,
    FOREIGN KEY (recipe_id)     REFERENCES "recipes"     (id) ON DELETE CASCADE,
    FOREIGN KEY (ingredient_id) REFERENCES "ingredients" (id) ON DELETE CASCADE
);

COMMIT;

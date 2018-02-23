BEGIN;

INSERT INTO glasses (name) VALUES ('coupe'), ('highball'), ('rocks');
INSERT INTO ingredient_classes (name) VALUES ('gin'), ('bourbon'), ('clubsoda'), ('misc');
INSERT INTO ingredients (class,name,amount,unit,notForRecipes) VALUES (1,'botanivore',1,'Ml750',False), (2,'bulleit',1,'Ml750',False), (4,'hot buttered rum mix',1,'recipe',False), (2,'pappy',1,'Ml750',True), (NULL,'hot water',1,'',False);
INSERT INTO recipes (name,garnish,instructions,for_ingredient_id) VALUES ('boozeland','','make some booze',NULL);
INSERT INTO recipes (name,garnish,instructions,for_ingredient_id) VALUES ('hot buttered rum mix','','heat on stove to combine',3);
INSERT INTO recipes_to_glasses (glass_id,recipe_id) VALUES (2,1);
INSERT INTO ingredients_to_recipes (recipe_id,ingredient_id,ingredient_class_id,amount_numer,amount_denom,unit) VALUES (1,1,NULL,1,2,'oz'),(2,NULL,1,1,3,'oz'),(2,2,NULL,4,1,'oz');
INSERT INTO purchases (ingredient,date,location,price) VALUES (1,DATE '2004-10-19','BevMo!',314),(1,DATE '2012-01-10','Cask!',12);

COMMIT;

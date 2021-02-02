INSERT INTO burgers (burger) VALUES
('Simple'),
('Double'),
('Triple')
ON CONFLICT DO NOTHING;

INSERT INTO toppings(topping) VALUES 
('Tomato'),
('Cheese'),
('Egg'),
('Onion'),
('Bacon'),
('Lettuce'),
('Pickle'),
('Mushroom'),
('Mayo'),
('Ketchup'),
('Mustard')
ON CONFLICT DO NOTHING;

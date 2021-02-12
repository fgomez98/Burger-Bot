INSERT INTO burgers (burger, cost) VALUES
('Simple', 5.0),
('Double', 7.0),
('Triple', 9.0)
ON CONFLICT DO NOTHING;

INSERT INTO toppings(topping, cost) VALUES 
('Tomato', 1.0),
('Cheese', 2.0),
('Egg', 1.0),
('Onion', 1.0),
('Bacon', 3.0),
('Lettuce', 1.5),
('Pickle', 1.5),
('Mushroom', 3.0),
('Mayo', 0.5),
('Ketchup', 0.5),
('Mustard', 0.5)
ON CONFLICT DO NOTHING;

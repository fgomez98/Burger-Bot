CREATE DATABASE telegram_bot_db;

\connect telegram_bot_db

create table if not exists burgers
(
    burger      text primary key,
    cost        double precision not null
);

create table if not exists toppings
(
    topping     text primary key,
    cost        double precision not null
);

create table if not exists clients
(   
    -- clientId        serial,
    clientId        integer,
    firstName       text not null,
    lastName        text not null,
    primary key (clientId)
);

create table if not exists orders
(
    orderId     serial primary key,
    clientId    integer not null,
    date        timestamp with time zone not null,
    completed   boolean default false,
    foreign key (clientId) references clients (clientId)
);

create table if not exists products
(
    productId   serial primary key,
    burger    text not null,
    foreign key (burger) references burgers (burger)        
);

create table if not exists products_toppings
(
    productId   integer not null,
    topping  text not null,
    amount      integer not null,
    foreign key (productId) references products (productId),
    foreign key (topping) references toppings (topping),
    primary key (topping, productId)
);

create table if not exists orders_products
(
    orderId     integer not null,
    productId   integer not null,
    cost        double precision not null,
    foreign key (orderId) references orders (orderId),
    foreign key (productId) references products (productId),
    primary key (orderId, productId)
);

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

-- usefull view 
create view orders_view 
    as select o.orderid as orderid, clientid, date, completed, sum(cost) as cost
        from orders as o join orders_products as op on o.orderid = op.orderid
        group by o.orderid, clientid, date, completed;
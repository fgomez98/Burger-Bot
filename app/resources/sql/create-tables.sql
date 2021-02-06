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
    completed   boolean default false
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

create view orders_view 
    as select o.orderid as orderid, clientid, date, completed, sum(cost) as cost
        from orders as o join orders_products as op on o.orderid = op.orderid
        group by o.orderid, clientid, date, completed;
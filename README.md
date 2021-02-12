# Burger Bot

On one of my visits to a burger restaurant in the current context of a pandemic, to reduce circulation and contact between people it was decided to take customer orders via WhatsApp. The implemented mechanism is a cool and fun way to order food, but I could not stop thinking on the employee constantly answering messages through his phone.

The adopted solution requires the acquisition of at least one telephone number and a smartphone which would be used by an employee to take orders and forward them to the kitchen. Availability is essential to provide good service.

I decided to take an approach by implementing a chatbot to take orders from clients. Throughout a web application employees will be able to track pending orders and mark them as completed once delivered. 

## Getting Started

### Prerequisites

This project uses the Stack tool to manage our Haskell project and install compiler and dependencies for us.
If you don't have stack installed you can run the following comand on Unix operating system.

```
curl -sSL https://get.haskellstack.org/ | sh
```

### Usage

Stack manages your GHC installations, to install GHC run:

```
stack setup
```

We need a few dependency libraries installed. To install dependencies and build project run:

```
stack build
```

We need to set up postgreSQL data base to persist data. Use app/resources/sql/init-db.sql file.

Before running Burger Bot we need to set up some environment variables.

```
export TOKEN_TELEGRAM=" *** insert bot token here *** "
DB_NAME="telegram_bot_db"
DB_HOST="localhost"
DB_USER=" *** insert db user here *** 
DB_PASSWD=" *** insert db password here *** 
```

We will be using postgreSQL 5432 default port.

Now we are ready

To run the Burger bot run:

```
stack exec burger-bot
```

To deploy the we application run:

```
stack exec web-app
```

## Some link's that where usefull to code this proyect 

* https://github.com/fizruk/telegram-bot-simple/
* https://www.youtube.com/watch?v=dfTTgdlsSzo&t=421s
* https://www.youtube.com/watch?v=dfTTgdlsSzo&t=421s
* https://github.com/keoko/postgresql-simple-examples
* https://www.lambda-land.com/posts/2017-11-16-postgresql-simple
* https://github.com/keoko/postgresql-simple-examples
* https://mmhaskell.com/blog/2018/9/17/simple-web-routing-with-spock
* https://jaspervdj.be/blaze/tutorial.html


## Acknowledgments

* [**Nikolai Kudasov**](https://github.com/fizruk)

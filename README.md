# Burger Bot



## Getting Started

### Prerequisites

This project uses the Stack tool to manage our Haskell project and install compiler and dependencies for us.
if you don't have stack installed you can run the following comand on Unix operating system

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

Before runing Burger Bot we need to save our bot token in an environment variable

```
export TOKEN_TELEGRAM=" *** insert bot token here *** "
```

## Some link's that where usefull to code this proyect 

https://github.com/fizruk/telegram-bot-simple/
https://www.youtube.com/watch?v=dfTTgdlsSzo&t=421s
https://www.youtube.com/watch?v=dfTTgdlsSzo&t=421s
https://github.com/keoko/postgresql-simple-examples
https://www.lambda-land.com/posts/2017-11-16-postgresql-simple
https://github.com/keoko/postgresql-simple-examples
https://mmhaskell.com/blog/2018/9/17/simple-web-routing-with-spock
https://jaspervdj.be/blaze/tutorial.html


## Acknowledgments

* [**Nikolai Kudasov**] (https://github.com/fizruk)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

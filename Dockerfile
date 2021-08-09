# FROM fpco/stack-build:lts-15.10  as build

# RUN mkdir /opt/build
# WORKDIR /opt/build

# # GHC dynamically links its compilation targets to lib gmp
# RUN apt-get update && apt-get download libgmp10
# RUN mv libgmp*.deb libgmp.deb

# COPY . /opt/build

# RUN stack build --system-ghc
# RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# # Base image for stack build so compiled artifact from previous
# # stage should run
# FROM ubuntu:16.04

# RUN mkdir -p /opt/app
# WORKDIR /opt/app

#  # Install lib gmp
# COPY --from=build /opt/build/libgmp.deb /tmp
# RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

# COPY --from=build /opt/build/bin .
# COPY .env .





# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-15.10 as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv 8B1DA6120C2BF624
RUN gpg --export --armor 8B1DA6120C2BF624 | sudo apt-key add -
RUN apt-get update 
RUN apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-15.10 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:20.04 as app

# Update the package lists:
RUN apt-get update

# Install the latest version of PostgreSQL.
# If you want a specific version, use 'postgresql-12' or similar instead of 'postgresql':
RUN apt-get -y install postgresql-client ca-certificates


COPY  /app/resources/bash/wait-for-it.sh /
RUN chmod 777 wait-for-it.sh

RUN mkdir -p /opt/app
WORKDIR /opt/app

# # Install postgresql 
# RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
# RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
# RUN apt-get update
# RUN apt-get -y install postgresql

# RUN apt-get update && apt-get -y install --assume-yes curl gcc libgmp-dev libpq-dev make xz-utils zlib1g-dev


# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
COPY .env .

EXPOSE 3000
# EXPOSE 5432
# CMD ["/opt/app/app", "8080"]

# https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker/
# https://gist.github.com/TimWSpence/269ab6943fbbaaf4b66374364f0051cd
# https://gist.github.com/TimWSpence/9b89b0915bf5224128e4b96abfd4ce02
# https://timwspence.github.io/blog/posts/2019-08-02-optimized-docker-builds-for-haskell-stack.html
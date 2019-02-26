# Freiburg2019

[![Build Status](https://travis-ci.org/timhabermaas/freiburg2019.svg?branch=master)](https://travis-ci.org/timhabermaas/freiburg2019)

## Installation

```sh
$ stack setup
$ stack build
```

## Run the server

```sh
$ createdb freiburg2019
$ env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/freiburg2019' stack exec freiburg2019-exe
```

### Environment variables

The following environment variables need to be set:

* `DATABASE_URL` (string): A PostgreSQL database connection string
* `PORT` (number): The port the web server should run on
* `ADMIN_PASSWORD` (string): The password for the _HTTP basic authentication_ protected admin area
* `SLEEPING_LIMIT` (number): The maximum number of people allowed to sleep in the gym/class rooms
* `CAMPING_LIMIT` (number): The maximum number of people allowed to camp

## Run the tests

```sh
$ createdb freiburg2019_test
$ env DATABASE_URL='postgres://localhost/freiburg2019_test' stack test
```

## Deployment

The application is currently deployed to Heroku. For deployment run:

```sh
$ git remote add heroku https://git.heroku.com/freiburg2019.git
$ git push heroku master
```

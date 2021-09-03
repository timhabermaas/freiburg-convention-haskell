# Freiburg Juggling Convention Registration

[![Build Status](https://travis-ci.org/timhabermaas/freiburg-convention.svg?branch=master)](https://travis-ci.org/timhabermaas/freiburg-convention)

## Installation

Install stack, see https://docs.haskellstack.org/en/stable/README.

```sh
$ stack setup
$ stack build
```

## Run the server

```sh
$ createdb freiburg_convention
$ env PORT=8080 OVERALL_LIMIT=130 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/freiburg_convention' stack run
```

### Environment variables

The following environment variables need to be set:

* `DATABASE_URL` (string): A PostgreSQL database connection string
* `PORT` (number): The port the web server should run on
* `ADMIN_PASSWORD` (string): The password for the _HTTP basic authentication_ protected admin area
* `OVERALL_LIMIT` (number): The maximum number of people allowed at the festival
* `SLEEPING_LIMIT` (number): The maximum number of people allowed to sleep in the gym/class rooms
* `CAMPING_LIMIT` (number): The maximum number of people allowed to camp

## Run the tests

```sh
$ createdb freiburg_convention_test
$ env DATABASE_URL='postgres://localhost/freiburg_convention_test' stack test
```

## Deployment

The app is deployed to [Heroku](https://www.heroku.com/).

### Create new Heroku instance

Requires the [Heroku CLI](https://devcenter.heroku.com/articles/heroku-cli) and the [Haskell Stack
buildkit](https://github.com/mfine/heroku-buildpack-stack):


```sh
$ heroku create freiburg2021
$ heroku buildpacks:set https://github.com/mfine/heroku-buildpack-stack
$ heroku addons:create heroku-postgresql:hobby-dev
```

### Setup deployment on different machine

This step is optional if you're using the same machine which ran `heroku create`.

```sh
$ git remote add heroku https://git.heroku.com/freiburg2021.git
```


### Deploy new version

```sh
$ git push heroku master
```

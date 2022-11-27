# Ontology Browser

A "fork" of the ontology browser from the [CO-ODE project](https://code.google.com/p/ontology-browser/) now hosted at https://github.com/co-ode-owl-plugins/ontology-browser.

This branch has mongo disabled

![Image of Ontology browser](docs/aardvark.png)

## Aims
* Make it easily buildable - maven
* Make it easily runnable - jetty (local) heroku (remote)
* Have it running again for demo purposes
* Make it more scalable - sort out stupid config problems
* Extract all rendering into views - jsp? or maybe just go full blown Spring MVC
* Tests!!

## Run locally

Ontology Browser uses MongoDB to store permalink information.
MongoDB should be running on localhost on the default port before starting.

Using maven to build.
Some dependencies are local (in /repo) as they do not exist on any mvn repo.

`mvn clean package cargo:run`

## Deployment to Heroku

Set the root ontology location and reasoner root ontology IRI:

    heroku config:set ONTOLOGY_ROOT_LOCATION=<your ontology URL>
    heroku config:set REASONING_ROOT_IRI=<IRI of the ontology to be reasoned with>

https://devcenter.heroku.com/articles/config-vars#using-the-heroku-cli

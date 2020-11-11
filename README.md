# Ontology Browser

A "fork" of the ontology browser from the [CO-ODE project](https://code.google.com/p/ontology-browser/) now hosted at https://github.com/co-ode-owl-plugins/ontology-browser.

Unfortunately, the mongo db implementation used for the
demo on heroku is no longer available (someone want to migrate to an alternative?)

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

Unfortunately, the mongo implementation used on heroku is no longer available and
I have no dev time to migrate to another store.

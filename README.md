# Ontology Browser

A "fork" of the ontology browser from the [CO-ODE project](https://code.google.com/p/ontology-browser/).

Example running on Heroku:
http://sheltered-savannah-5966.herokuapp.com/individuals/67989612/?session=14c4e36af7d-0-14c4e36ff34

## Aims
* Make it easily buildable - maven
* Make it easily runnable - jetty (local) heroku (remote)
* Have it running again for demo purposes
* Make it more scalable - sort out stupid config problems
* Extract all rendering into views - jsp? or maybe just go full blown Spring MVC
* Tests!!

## Build

Using maven to build.
Some dependencies are local (in /repo) as they do not exist on any mvn repo.

`mvn clean package cargo:run`

## Deployment

`git push heroku master`

See [Heroku instructions for Jetty](https://devcenter.heroku.com/articles/deploy-a-java-web-application-that-launches-with-jetty-runner).

http://sheltered-savannah-5966.herokuapp.com/individuals/67989612/?session=14c4e36af7d-0-14c4e36ff34
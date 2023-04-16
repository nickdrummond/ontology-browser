# Ontology Browser

The browser loads a single ontology (and its imports) on startup and 
renders it for easy navigation.

![Image of Ontology browser](docs/classes.png)

## Features
* Navigation of all entities (classes, properties, individuals and datatypes)
* Hierarchies
* Entity usage
* Manchester OWL Syntax rendering
* Ontology metrics
* Searching
* DL Query (with set subtraction)

## Run

### Maven

`mvn clean jetty:run`

Then navigate to http://localhost:8080

This will give you a browser with a tiny example ontology.

### Docker

Run the Dockerfile, which will build from github by default and create an image
based on [Jetty 10](https://hub.docker.com/_/jetty/)  Alpine

Run the image and navigate to http://localhost:8080

### Configuration

To load your own, set the root ontology location and reasoner root ontology IRI environment variables:

    ONTOLOGY_ROOT_LOCATION=<your ontology URL>
    REASONING_ROOT_IRI=<IRI of the ontology to be reasoned with>
    PROJECT_NAME=<Ontology name>
    PROJECT_URL=<Project documentation url>
    PROJECT_TAGLINE=<Project tagline text>
    PROJECT_CONTACT=<Email address of contact>

## Notes

This is a "fork" of the ontology browser from the [CO-ODE project](https://code.google.com/p/ontology-browser/) now hosted at https://github.com/co-ode-owl-plugins/ontology-browser.

Unlike v1, there is no storing of sessions/multi config.

## Implementation

This is implemented using Java 11, Spring MVC, [OWLAPI](https://github.com/owlcs/owlapi), Thymeleaf.

Some dependencies are local (in /repo) as they do not exist on any mvn repo.

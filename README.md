# Ontology Browser

The browser loads a single OWL ontology (and its imports) on startup and 
renders it for easy navigation.

[Example using the Star Wars Ontology](https://star-wars-ontology.up.railway.app/) running on railway.app

![Image of Ontology browser](docs/astromech.png)

## Features
* Navigation of all entities (classes, properties, individuals and datatypes)
* Hierarchies, subclass/property/relations/annotations
* Entity usage
* Manchester OWL Syntax rendering
* Ontology metrics
* Searching
* DL Query (with set subtraction)
* Axioms view (and search)
* Paging
* Dark mode

![Image of Ontology browser - dark mode](docs/obi-wan.png)

## Run

### Maven

`mvn clean jetty:run`

Then navigate to http://localhost:8080

This will give you a browser with a tiny example ontology.

### Docker

Run the Dockerfile, which will build from github by default and create an image
based on [Jetty 10](https://hub.docker.com/_/jetty/)  Alpine

Run the image and navigate to http://localhost:8080

## Deploy

### railway.app

You can deploy this on [railway.app](https://railway.app) with the button below and be up and running with just a few config variables.
[![Deploy on Railway](https://railway.app/button.svg)](https://railway.app/template/hI-_yx?referralCode=-eh5ht)

### Configuration

To load your own, set the root ontology location and reasoner root ontology IRI environment variables:

    ONTOLOGY_ROOT_LOCATION=<your ontology URL>
    REASONING_ROOT_IRI=<IRI of the ontology to be reasoned with>
    PROJECT_NAME=<Ontology name>
    PROJECT_URL=<Project documentation url>
    PROJECT_TAGLINE=<Project tagline text>
    PROJECT_CONTACT=<Email address of contact>
    LABEL_IRI=<IRI of the annotation property to use for rendering>
    RESTART_SECRET=<used for /restart endpoint secret param>

## SKOS/SKOS-XL support

The OWL API treats SKOS Concepts as individuals and their properties as annotations.

### Hierarchy
You can see the broader/narrower tree in the `individuals by Annotation` view
![Image of SKOS support](docs/skos.png)

### Labels/rendering
For SKOS-XL terminologies, set LABEL_IRI=http://www.w3.org/2008/05/skos-xl#prefLabel
Concept "C" will be rendered as "C Label" if the conventional SKOS-XL reified labelling is used in the loaded terminology:

    C Type skos:Concept
    l Type skosxl:Label
    C -> skosxl:prefLabel -> l
    l -> skosxl:literalForm -> "C Label"

## Implementation

This is implemented using Java 20, Spring MVC, [OWLAPI](https://github.com/owlcs/owlapi) and Thymeleaf.

Some dependencies are local (in /repo) as they do not exist on any mvn repo.


## Status

This is a personal project and all development is on a purely "as time allows" basis.

There is no release schedule or plan as I currently have no known users.
If you wish to use this project in a production environment, please consider contacting me and considering
funding development and support. Alternatively, you may fork the project and maintain it yourself.

This is a "fork" of the ontology browser from the [CO-ODE project](https://code.google.com/p/ontology-browser/) now hosted at https://github.com/co-ode-owl-plugins/ontology-browser.

Unlike v1, there is no storing of sessions/multi config.

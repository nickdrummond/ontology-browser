package org.coode.www.service.hierarchy;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLProperty;

public record Relation<P extends OWLProperty>(P property, OWLNamedIndividual individual) {
}

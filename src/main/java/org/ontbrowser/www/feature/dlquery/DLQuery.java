package org.ontbrowser.www.feature.dlquery;

import org.semanticweb.owlapi.model.OWLClassExpression;

public record DLQuery(OWLClassExpression owlClassExpression, QueryType queryType) { }

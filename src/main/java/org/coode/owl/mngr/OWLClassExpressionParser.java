package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.OWLClassExpression;

public interface OWLClassExpressionParser {

    OWLClassExpression parse(String str);
}

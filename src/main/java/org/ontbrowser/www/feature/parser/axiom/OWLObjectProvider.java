package org.ontbrowser.www.feature.parser.axiom;

import org.semanticweb.owlapi.model.*;

import java.util.List;

public interface OWLObjectProvider {

    OWLObjectPropertyExpression objPropExpr(String key);

    OWLObjectProperty objProp(String key);

    OWLDataPropertyExpression dataPropExpr(String key);

    OWLDataProperty dataProp(String key);

    OWLAnnotationProperty annotProp(String key);

    OWLNamedIndividual ind(String key);

    OWLClass cls(String key);

    OWLLiteral lit(String key);

    OWLClassExpression clsExpr(String key);

    OWLDatatype datatype(String key);

    <T> List<T> list(String key, Class<T> clz);
}

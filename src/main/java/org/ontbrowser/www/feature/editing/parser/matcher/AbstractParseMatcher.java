package org.ontbrowser.www.feature.editing.parser.matcher;

import org.ontbrowser.www.feature.editing.parser.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;

import java.util.List;

public abstract class AbstractParseMatcher<T> {

    public T get() {
        throw new RuntimeException("Getter not implemented");
    }

    public OWLNamedIndividual getIndividual() {
        throw new RuntimeException("Not an individual matcher");
    }

    public OWLClass getOWLClass() {
        throw new RuntimeException("Not a class matcher");
    }

    public OWLClassExpression getOWLClassExpression() {
        throw new RuntimeException("Not a class expression matcher");
    }

    public OWLObjectProperty getObjectProperty() {
        throw new RuntimeException("Not an object property matcher");
    }

    public OWLDataProperty getDataProperty() {
        throw new RuntimeException("Not a data property matcher");
    }

    public OWLAnnotationProperty getAnnotationProperty() {
        throw new RuntimeException("Not an annotation property matcher");
    }

    public OWLLiteral getLiteral() {
        throw new RuntimeException("Not a literal matcher");
    }

    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
    }

    public OWLObjectPropertyExpression getObjectPropertyExpression() {
        throw new RuntimeException("Not an object property expression matcher");
    }

    public OWLDataPropertyExpression getDataPropertyExpression() {
        throw new RuntimeException("Not a data property expression matcher");
    }

    public OWLDatatype getDatatype() {
        throw new RuntimeException("Not a datatype matcher");
    }

    public List<T> getObjectList(Class<T> clz) {
        throw new RuntimeException("Not a list matcher");
    }
}

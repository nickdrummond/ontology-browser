package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLDataFactory;

import java.util.Collections;

public class AnnotationPropMatcher extends AbstractParseMatcher<OWLAnnotationProperty> {

    private OWLAnnotationProperty prop;

    @Override
    public OWLAnnotationProperty get() {
        return prop;
    }

    @Override
    public OWLAnnotationProperty getAnnotationProperty() {
        return prop;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLAnnotationProperty prop = checker.getOWLAnnotationProperty(tokenizer.consumeNext());
        if (prop == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, false, true, false, Collections.emptySet());
        }
        this.prop = prop;
    }
}

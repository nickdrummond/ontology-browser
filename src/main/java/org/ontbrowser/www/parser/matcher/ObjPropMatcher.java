package org.ontbrowser.www.parser.matcher;

import org.ontbrowser.www.parser.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;

import java.util.Collections;

public class ObjPropMatcher extends AbstractParseMatcher<OWLObjectProperty> {
    private OWLObjectProperty prop;

    @Override
    public OWLObjectProperty get() {
        return prop;
    }

    @Override
    public OWLObjectProperty getObjectProperty() {
        return prop;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLObjectProperty prop = checker.getOWLObjectProperty(tokenizer.consumeNext());
        if (prop == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, true, false, false, false, false, false, Collections.emptySet());
        }
        this.prop = prop;
    }
}

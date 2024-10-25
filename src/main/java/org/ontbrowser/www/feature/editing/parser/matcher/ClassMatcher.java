package org.ontbrowser.www.feature.editing.parser.matcher;

import org.ontbrowser.www.feature.editing.parser.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;

import java.util.Collections;

public class ClassMatcher extends AbstractParseMatcher<OWLClass> {
    private OWLClass cls;

    @Override
    public OWLClass get() {
        return cls;
    }

    @Override
    public OWLClass getOWLClass() {
        return cls;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLClass cls = checker.getOWLClass(tokenizer.consumeNext());
        if (cls == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, true, false, false, false, false, false, false, Collections.emptySet());
        }
        this.cls = cls;
    }
}

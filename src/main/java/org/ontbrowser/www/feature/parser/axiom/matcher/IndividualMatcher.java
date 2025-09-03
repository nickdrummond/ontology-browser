package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import java.util.Collections;

public class IndividualMatcher extends AbstractParseMatcher<OWLNamedIndividual> {

    private OWLNamedIndividual ind;

    @Override
    public OWLNamedIndividual get() {
        return ind;
    }

    @Override
    public OWLNamedIndividual getIndividual() {
        return ind;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLNamedIndividual ind = checker.getOWLIndividual(tokenizer.consumeNext());
        if (ind == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, true, false, false, false, Collections.emptySet());
        }
        this.ind = ind;
    }
}

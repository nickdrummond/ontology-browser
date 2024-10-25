package org.ontbrowser.www.feature.editing.parser.matcher;

import org.ontbrowser.www.feature.editing.parser.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import java.util.Collections;

public class IndividualListMatcher extends OWLObjectListMatcher<OWLNamedIndividual> {

    public IndividualListMatcher(String separator) {
        super(separator);
    }

    @Override
    protected OWLNamedIndividual create(String token, int pointer, MyTokenizer tokenizer, OWLEntityChecker checker) {
        OWLNamedIndividual ind = checker.getOWLIndividual(token);
        // would be better if the exception could be generated in the abstract
        if (ind == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, true, false, false, false, Collections.emptySet());
        }
        return ind;
    }
}

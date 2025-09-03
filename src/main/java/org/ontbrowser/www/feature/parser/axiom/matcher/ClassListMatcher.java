package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;

import java.util.Collections;

public class ClassListMatcher extends OWLObjectListMatcher<OWLClassExpression> {

    public ClassListMatcher(String separator) {
        super(separator);
    }

    @Override
    protected OWLClassExpression create(String token, int pointer, MyTokenizer tokenizer, OWLEntityChecker checker) {
        OWLClassExpression clsExpr = checker.getOWLClass(token);
        // would be better if the exception could be generated in the abstract
        if (clsExpr == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, true, false, false, false, false, false, false, Collections.emptySet());
        }
        return clsExpr;

    }
}

package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;

import java.util.Collections;

public class DataPropExpressionMatcher extends AbstractParseMatcher<OWLDataPropertyExpression> {

    private OWLDataPropertyExpression prop;

    @Override
    public OWLDataPropertyExpression get() {
        return prop;
    }

    @Override
    public OWLDataPropertyExpression getDataPropertyExpression() {
        return prop;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLDataPropertyExpression prop = checker.getOWLDataProperty(tokenizer.consumeNext());
        if (prop == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, true, false, false, false, false, Collections.emptySet());
        }
        this.prop = prop;
    }
}

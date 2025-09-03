package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;

import java.util.Collections;

public class DatatypeMatcher extends AbstractParseMatcher<OWLDatatype> {

    private OWLDatatype dt;

    @Override
    public OWLDatatype get() {
        return dt;
    }

    @Override
    public OWLDatatype getDatatype() {
        return dt;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        OWLDatatype dt = checker.getOWLDatatype(tokenizer.consumeNext());
        if (dt == null) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, true, false, false, Collections.emptySet());
        }
        this.dt = df.getOWLDatatype(dt);
    }
}

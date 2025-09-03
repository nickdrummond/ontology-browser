package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;

import java.util.Set;

public class SugarMatcher extends AbstractParseMatcher<String> {
    private final String syntax;

    @Override
    public String get() {
        return syntax;
    }

    public SugarMatcher(String syntax) {
        this.syntax = syntax;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        if (!tokenizer.consumeNext().equalsIgnoreCase(syntax)) {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, false, false, false, Set.of(syntax));
        }
    }
}

package org.ontbrowser.www.parser.matcher;

import org.ontbrowser.www.parser.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;

import java.util.Collections;

import static org.ontbrowser.www.util.MyStringUtils.stripQuotes;

public class QuotedStringMatcher extends AbstractParseMatcher<String> {

    private String value;

    @Override
    public String get() {
        return value;
    }

    public String getValue() {
        return value;
    }

    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        String s = tokenizer.consumeNext();
        if (s.startsWith("\"") && s.endsWith("\"")) {
            this.value = stripQuotes(s);
        }
        else {
            throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, false, false,false, Collections.emptySet());
        }
    }
}

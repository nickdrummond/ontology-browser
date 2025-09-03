package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.ArrayList;
import java.util.List;

public abstract class OWLObjectListMatcher<T extends OWLObject> extends AbstractParseMatcher<T> {
    private final String separator;
    private final List<T> objects = new ArrayList<>();

    protected OWLObjectListMatcher(String separator) {
        this.separator = separator;
    }

    @Override
    public List<T> getObjectList(Class<T> clz) {
        return objects;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        while (tokenizer.hasMore()) {
            this.objects.add(parseOWLObject(tokenizer, checker));
        }
    }

    private T parseOWLObject(MyTokenizer tokenizer, OWLEntityChecker checker) {
        int pointer = tokenizer.getPointer();
        String token = tokenizer.consumeNext();
        if (token.endsWith(separator)) {
            token = token.substring(0, token.length()-1);
        }
        return create(token, pointer, tokenizer, checker);
    }

    protected abstract T create(String token, int pointer, MyTokenizer tokenizer, OWLEntityChecker checker);
}

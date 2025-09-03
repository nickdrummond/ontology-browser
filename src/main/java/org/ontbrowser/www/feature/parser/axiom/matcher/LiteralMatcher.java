package org.ontbrowser.www.feature.parser.axiom.matcher;

import org.ontbrowser.www.feature.parser.axiom.MyTokenizer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLLiteral;

import java.util.Collections;

import static org.ontbrowser.www.util.MyStringUtils.stripQuotes;

public class LiteralMatcher extends AbstractParseMatcher<OWLLiteral> {

    public static final String DT_MARKER = "^^";
    private OWLLiteral lit;

    @Override
    public OWLLiteral get() {
        return lit;
    }

    @Override
    public OWLLiteral getLiteral() {
        return lit;
    }

    @Override
    public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
        int pointer = tokenizer.getPointer();
        String s = tokenizer.consumeNext();

        if (s.startsWith("\"") && s.endsWith("\"")) {

            String value = stripQuotes(s);

            if (tokenizer.remainder().startsWith("@")) { // lang literal
                String lang = tokenizer.consumeNext().substring(1);
                this.lit = df.getOWLLiteral(value, lang);
                return;
            }

            if (tokenizer.remainder().startsWith(DT_MARKER)) { // typed literal
                pointer = tokenizer.getPointer() + DT_MARKER.length();
                String datatypeS = tokenizer.consumeNext().substring(DT_MARKER.length());
                OWLDatatype dt = checker.getOWLDatatype(datatypeS);
                if (dt != null) {
                    this.lit = df.getOWLLiteral(value, dt);
                    return;
                }
                throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, true, false, false, Collections.emptySet());
            }

            this.lit = df.getOWLLiteral(value);
            return;
        }

        if (s.equals("true")) {
            this.lit = df.getOWLLiteral(true);
            return;
        } else if (s.equals("false")) {
            this.lit = df.getOWLLiteral(false);
            return;
        }

        try {
            int i = Integer.parseInt(s);
            this.lit = df.getOWLLiteral(i); // integer literal
            return;
        } catch (NumberFormatException e) {
            // not an int
        }
        try {
            float f = Float.parseFloat(s);
            this.lit = df.getOWLLiteral(f);
            return;
        } catch (NumberFormatException ef) {
            // not a float
        }
        try {
            double d = Double.parseDouble(s);
            this.lit = df.getOWLLiteral(d);
            return;
        } catch (NumberFormatException ef) {
            // not a double
        }

        throw new ParserException(tokenizer.tokens(), pointer, 0, pointer+1, false, false, false, false, false, false, false, false, Collections.emptySet());
    }
}

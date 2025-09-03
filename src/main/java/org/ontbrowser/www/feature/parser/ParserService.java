package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.feature.parser.axiom.MOSAxiomTreeParser;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.springframework.stereotype.Service;

/**
 * Manchester OWL Syntax parser.
 */
@Service
public class ParserService {

    public OWLClassExpression getOWLClassExpression(final String expression,
                                                    final OWLDataFactory df,
                                                    final OWLEntityChecker owlEntityChecker) {
        var parser = new ManchesterOWLSyntaxClassExpressionParser(df, owlEntityChecker);
        return parser.parse(makeSafe(expression));
    }

    public OWLAxiom parseAxiom(String axiom, OWLDataFactory df, OWLEntityChecker checker) throws ParserException {
        return new MOSAxiomTreeParser(df, checker).parse(axiom);
    }

    // Apostrophes are not parsed correctly without escaping
    // May cause problems for error message offsets?
    private String makeSafe(String expression) {
        return expression.replace("'", "\\\\'");
    }

    /**
     * Wraps result in Gaphu ParseResult or ParseException for client processing.
     */
    public ParseResultJson parse(
            final String expression,
            final OWLDataFactory df,
            final OWLEntityChecker owlEntityChecker) {
        try {
            getOWLClassExpression(expression, df, owlEntityChecker);
            return ParseResultJson.ok(expression);
        } catch (ParserException e) {
            return ParseResultJson.error(expression, e);
        }
    }

    public ParseResultJson parseAxiomResult(String expression, OWLDataFactory df, OWLEntityChecker checker) {
        try {
            parseAxiom(expression, df, checker);
            return ParseResultJson.ok(expression);
        } catch (ParserException e) {
            return ParseResultJson.error(expression, e);
        }
    }
}

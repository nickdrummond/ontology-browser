package org.ontbrowser.www.feature.dlquery;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.feature.expression.AutocompleteService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nickdrummond.parsejs.AutocompleteResult;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

/**
 * Manchester OWL Syntax parser.
 */
@Service
public class ParserService {

    private final AutocompleteService autocompleteService;

    public ParserService(@Autowired AutocompleteService autocompleteService) {
        this.autocompleteService = autocompleteService;
    }

    public OWLClassExpression getOWLClassExpression(final String expression,
                                                    final OWLDataFactory df,
                                                    final OWLEntityChecker owlEntityChecker) {
        ManchesterOWLSyntaxClassExpressionParser parser =
                new ManchesterOWLSyntaxClassExpressionParser(df, owlEntityChecker);

        return parser.parse(makeSafe(expression));
    }

    // Apostrophes are not parsed correctly without escaping
    // May cause problems for error message offsets?
    private String makeSafe(String expression) {
        return expression.replace("'", "\\\\'");
    }

    /**
     * Wraps result in Gaphu ParseResult or ParseException for client processing.
     */
    public ParseResult parse(final String expression,
                             final OWLDataFactory df,
                             final OWLEntityChecker owlEntityChecker) throws ParseException {
        try {
            getOWLClassExpression(expression, df, owlEntityChecker);
            return new ParseResult(expression, "OK");
        } catch (ParserException e) {
            throw new ParseException(expression, e.getMessage(), e.getStartPos(), e.getCurrentToken());
        }
    }

    /**
     * Wraps results in Gaphu AutocompleteResult for client processing.
     */
    public AutocompleteResult autocomplete(final String expression,
                                           final OWLDataFactory df,
                                           final OWLEntityChecker owlEntityChecker,
                                           final OWLEntityFinder finder,
                                           final ShortFormProvider sfp) {

        try {
            getOWLClassExpression(expression, df, owlEntityChecker);
            throw new RuntimeException("Cannot get here if we have correctly forced an error");
        } catch (ParserException e) {
            return autocompleteService.exceptionToAutocomplete(expression, e, finder, sfp);
        }
    }
}

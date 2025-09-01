package org.ontbrowser.www.feature.dlquery;

import org.ontbrowser.www.feature.expression.AutocompleteResultJson;
import org.ontbrowser.www.feature.expression.AutocompleteService;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * Manchester OWL Syntax parser.
 */
@Service
public class ParserService {

    private final AutocompleteService autocompleteService;

    public ParserService(AutocompleteService autocompleteService) {
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
    public ParseResultJson parse(
            final String expression,
            final OWLDataFactory df,
            final OWLEntityChecker owlEntityChecker) {
        try {
            getOWLClassExpression(expression, df, owlEntityChecker);
            return new ParseResultJson(ParseStatus.OK, expression,  "OK", 0, "");
        } catch (ParserException e) {
            return new ParseResultJson(ParseStatus.ERROR, expression, e.getMessage(), e.getStartPos(), e.getCurrentToken());
        }
    }

    /**
     * Wraps results in Gaphu AutocompleteResult for client processing.
     */
    public AutocompleteResultJson autocomplete(final String expression,
                                               final OWLDataFactory df,
                                               final OWLEntityChecker owlEntityChecker,
                                               final OWLEntityFinder finder,
                                               final ShortFormProvider sfp) {

        try {
            getOWLClassExpression(expression, df, owlEntityChecker);
            // TODO it DOES get here if "hadSibling some " as it returns a filler of owl:Thing
            return new AutocompleteResultJson(expression, 0, "", Map.of());
        } catch (ParserException e) {
            return autocompleteService.exceptionToAutocomplete(expression, e, finder, sfp);
        }
    }
}

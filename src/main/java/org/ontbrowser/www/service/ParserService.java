package org.ontbrowser.www.service;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.parser.MOSAxiomTreeParser;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntax;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;
import uk.co.nickdrummond.parsejs.AutocompleteResult;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

import java.util.*;

/**
 * Manchester OWL Syntax parser.
 */
@Service
public class ParserService {

    private static final String ERROR_TOKEN = "$$";

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

    public OWLAxiom parseAxiom(String axiom, OWLDataFactory df, OWLEntityChecker checker) throws ParseException {
        try {
            return new MOSAxiomTreeParser(df, checker).parse(axiom);
        } catch (ParserException e) {
            throw new ParseException(axiom, e.getMessage(), e.getStartPos(), e.getCurrentToken());
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
            return exceptionToAutocomplete(expression, e, finder, sfp);
        }
    }

    public AutocompleteResult autocompleteAxiom(final String axiom,
                                           final OWLDataFactory df,
                                           final OWLEntityChecker owlEntityChecker,
                                           final OWLEntityFinder finder,
                                           final ShortFormProvider sfp) {

        try {
            new MOSAxiomTreeParser(df, owlEntityChecker).parse(axiom);
            throw new RuntimeException("Cannot get here if we have correctly forced an error " + axiom);
        } catch (ParserException e) {
            return exceptionToAutocomplete(axiom, e, finder, sfp);
        }
    }

    // TODO review if the OWLAPI even produces $$ token now
    private AutocompleteResult exceptionToAutocomplete(final String expression,
                                                       final ParserException e,
                                                       final OWLEntityFinder finder,
                                                       final ShortFormProvider sfp) {

        String lastToken;
        int pos;
        if (e.getCurrentToken().endsWith(ERROR_TOKEN)){
            lastToken = e.getCurrentToken();
            lastToken = lastToken.substring(0, lastToken.length()-ERROR_TOKEN.length()); // remove the $$
            pos = e.getStartPos();
        }
        else{
            lastToken = e.getTokenSequence().get(e.getTokenSequence().size()-1);
            if (lastToken.endsWith(ERROR_TOKEN)){
                lastToken = lastToken.substring(0, lastToken.length()-ERROR_TOKEN.length()); // remove the $$
            }
            else if (lastToken.equals("|EOF|")){
                lastToken = e.getTokenSequence().get(e.getTokenSequence().size()-2); // EOF is last
            }
            else if (!expression.endsWith(lastToken)){ // later invalid tokens are not in the list
                lastToken = expression.substring(expression.lastIndexOf(" ")+1); // we just have to guess at the last word
            }
            pos = expression.length() - lastToken.length();
        }

        Map<String, List<String>> expected = new HashMap<>();

        String search = lastToken + ".*"; // starts with

        if (pos == e.getStartPos()){ // then the error is the last token, and we can determine the type

            if (hasExpectedToken(e)){
                final Set<String> keywords = e.getExpectedKeywords();
                if (!keywords.isEmpty()){
                    List<String> matchingKeywords = new ArrayList<>();
                    for (String keyword : keywords){
                        if (lastToken.length() == 0 || keyword.startsWith(lastToken)){
                            matchingKeywords.add(keyword);
                        }
                    }
                    expected.put("keyword", matchingKeywords);
                }

                if (e.isClassNameExpected()){
                    addResults(expected, EntityType.CLASS, finder.getOWLClasses(search), sfp);
                }
                if (e.isObjectPropertyNameExpected()){
                    addResults(expected, EntityType.OBJECT_PROPERTY, finder.getOWLObjectProperties(search), sfp);
                }
                if (e.isDataPropertyNameExpected()){
                    addResults(expected, EntityType.DATA_PROPERTY, finder.getOWLDataProperties(search), sfp);
                }
                if (e.isDatatypeNameExpected()){
                    addResults(expected, EntityType.DATATYPE, finder.getOWLDatatypes(search), sfp);
                }
                if (e.isAnnotationPropertyNameExpected()){
                    addResults(expected, EntityType.ANNOTATION_PROPERTY, finder.getOWLAnnotationProperties(search), sfp);
                }
                if (e.isIndividualNameExpected()){
                    addResults(expected, EntityType.NAMED_INDIVIDUAL, finder.getOWLIndividuals(search), sfp);
                }
            }
            else{
                expected.put("literal", Collections.emptyList());
            }
        }
        else{
            addKeywords(expected, lastToken);
            addResults(expected, EntityType.CLASS, finder.getOWLClasses(search), sfp);
            addResults(expected, EntityType.OBJECT_PROPERTY, finder.getOWLObjectProperties(search), sfp);
            addResults(expected, EntityType.DATA_PROPERTY, finder.getOWLDataProperties(search), sfp);
            addResults(expected, EntityType.DATATYPE, finder.getOWLDatatypes(search), sfp);
            addResults(expected, EntityType.ANNOTATION_PROPERTY, finder.getOWLAnnotationProperties(search), sfp);
            addResults(expected, EntityType.NAMED_INDIVIDUAL, finder.getOWLIndividuals(search), sfp);
        }

        return new AutocompleteResult(expression, pos, lastToken, expected);
    }


    private boolean hasExpectedToken(ParserException e) {
        return !e.getExpectedKeywords().isEmpty() ||
                e.isClassNameExpected() ||
                e.isObjectPropertyNameExpected() ||
                e.isDataPropertyNameExpected() ||
                e.isIndividualNameExpected() ||
                e.isDatatypeNameExpected() ||
                e.isAnnotationPropertyNameExpected();
    }

    private void addKeywords(Map<String, List<String>> map, String token){
        List<String> names = new ArrayList<>();

        for (ManchesterOWLSyntax keyword : ManchesterOWLSyntax.values()){
            if ((keyword.isClassExpressionConnectiveKeyword() ||
                    keyword.isClassExpressionQuantiferKeyword()) &&
                            (keyword.toString().startsWith(token))){
                    names.add(keyword.toString());

            }
        }
        map.put("keyword", names);
    }

    private <T extends OWLEntity> void addResults(final Map<String, List<String>> map,
                            final EntityType<T> cls,
                            final Collection<T> matches,
                            final ShortFormProvider sfp) {
        List<String> names = new ArrayList<>();
        for (OWLEntity match : matches){
            String name = sfp.getShortForm(match);
            if (name.contains(" ")){
                name = "\"" + name + "\"";
            }
            names.add(name);
        }
        map.put(cls.getName(), names);
    }
}

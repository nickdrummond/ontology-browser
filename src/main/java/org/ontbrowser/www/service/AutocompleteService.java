package org.ontbrowser.www.service;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntax;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;
import uk.co.nickdrummond.parsejs.AutocompleteResult;

import java.util.*;

@Service
public class AutocompleteService {

    private static final String ERROR_TOKEN = "$$";

    // TODO review if the OWLAPI even produces $$ token now
    public AutocompleteResult exceptionToAutocomplete(final String expression,
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

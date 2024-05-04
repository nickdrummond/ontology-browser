package org.ontbrowser.www.renderer;

import org.eclipse.rdf4j.model.vocabulary.SKOSXL;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.SKOSVocabulary;

import javax.annotation.Nonnull;
import java.util.*;

/**
 * A ShortFormProvider that renders SKOS Concepts with reified labels.
 * For label instances, it renders uses literalForm
 * For Concept instances it renders the literalForm of the instance in its prefLabel annotation
 */
public class SkosXLShortFormProvider implements ShortFormProvider {

    private final AnnotationValueShortFormProvider delegate;
    private final String lang;
    private final Set<OWLOntology> ontologies;
    private final OWLDataFactory df;

    private final OWLAnnotationProperty prefLabel;
    private final OWLAnnotationProperty literalForm;
    private final OWLClass skosConcept;

    public SkosXLShortFormProvider(final String lang,
                                   final Set<OWLOntology> ontologies,
                                   final ShortFormProvider defaultSFP) {
        this.lang = lang;
        this.ontologies = ontologies;
        this.df = ontologies.iterator().next().getOWLOntologyManager().getOWLDataFactory();

        prefLabel = df.getOWLAnnotationProperty(SKOSXL.PREF_LABEL.toString());

        literalForm = df.getOWLAnnotationProperty(SKOSXL.LITERAL_FORM.toString());

        skosConcept = df.getOWLClass(SKOSVocabulary.CONCEPT.getIRI());

        delegate = new AnnotationValueShortFormProvider(
                Collections.singletonList(literalForm),
                createLangMap(literalForm, lang),
                ontologies::stream,
                defaultSFP);
    }

    @Nonnull
    public String getShortForm(@Nonnull OWLEntity owlEntity) {
        if (isSkosConcept(owlEntity)) {
            Optional<OWLIndividual> label = getLabelInstance(owlEntity.asOWLNamedIndividual());
            if (label.isPresent()) {
                if (label.get().isAnonymous()) {
                    return getShortForm(label.get().asOWLAnonymousIndividual());
                }
                return delegate.getShortForm(label.get().asOWLNamedIndividual());
            }
        }
        return delegate.getShortForm(owlEntity);
    }

    private String getShortForm(OWLAnonymousIndividual anonymousIndividual) {
        String defaultNoLang = null;
        for (OWLOntology o : ontologies) {
            for (OWLAnnotationAssertionAxiom annot : o.getAnnotationAssertionAxioms(anonymousIndividual)) {
                if (annot.getProperty().equals(literalForm)) {
                    OWLAnnotationValue value = annot.getValue();
                    Optional<OWLLiteral> lit = value.asLiteral();
                    if (lit.isPresent()) {
                        if (lit.get().getLang().equals(lang)) {
                            return lit.get().getLiteral();
                        }
                        else if (!lit.get().hasLang()) {
                            defaultNoLang = lit.get().getLiteral();
                        }
                    }
                }
            }
        }
        return (defaultNoLang != null) ? defaultNoLang : anonymousIndividual.toStringID();
    }

    private Optional<OWLIndividual> getLabelInstance(OWLNamedIndividual ind) {
        for (OWLOntology o : ontologies) {
            for (OWLAnnotationAssertionAxiom annot : o.getAnnotationAssertionAxioms(ind.getIRI())) {
                if (annot.getProperty().equals(prefLabel)) {
                    Optional<IRI> value = annot.getValue().asIRI();
                    // TODO should check if there is an individual with this IRI
                    if (value.isPresent()) {
                        return Optional.of(df.getOWLNamedIndividual(value.get()));
                    }
                    else {
                        Optional<OWLAnonymousIndividual> labelInstance = annot.getValue().asAnonymousIndividual();
                        if (labelInstance.isPresent()) {
                            return Optional.of(labelInstance.get());
                        }
                    }
                }
            }
        }
        return Optional.empty();
    }

    private boolean isSkosConcept(OWLEntity owlEntity) {
        if (owlEntity.isOWLNamedIndividual()) {
            for (OWLOntology o : ontologies) {
                if (o.containsAxiom(df.getOWLClassAssertionAxiom(skosConcept, owlEntity.asOWLNamedIndividual()))) {
                    return true;
                }
            }
        }
        return false;
    }

    public void dispose() {
        delegate.dispose();
    }

    private <P> Map<P, List<String>> createLangMap(P p, String lang) {
        final Map<P, List<String>> lMap = new HashMap<>();
        if (lang.length() > 0){
            List<String> langs = new ArrayList<>();
            langs.add(lang);
            langs.add(""); // default to no language
            lMap.put(p, langs);
        }
        return lMap;
    }
}

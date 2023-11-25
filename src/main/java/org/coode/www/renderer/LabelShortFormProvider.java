package org.coode.www.renderer;

import org.eclipse.rdf4j.model.vocabulary.SKOSXL;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.SKOSVocabulary;

import javax.annotation.Nonnull;
import java.util.*;

/**
 * A shortformProvider that uses the server properties to render
 * 1) An annotation value on the label (in the given language), if available otherwise
 * 2) uses the default shortFormProvider provided
 */
public class LabelShortFormProvider implements ShortFormProvider {

    private final AnnotationValueShortFormProvider delegate;
    private final boolean reifyRendering;
    private final Set<OWLOntology> ontologies;
    private final OWLDataFactory df;

    public LabelShortFormProvider(final OWLAnnotationProperty annotProp,
                                  final String lang,
                                  final Set<OWLOntology> ontologies,
                                  final ShortFormProvider defaultSFP) {
        this.ontologies = ontologies;
        this.df = ontologies.iterator().next().getOWLOntologyManager().getOWLDataFactory();

        reifyRendering = isSkosXLLabelAnnotation(annotProp);
        if (reifyRendering) {
            OWLAnnotationProperty literalForm = df.getOWLAnnotationProperty(SKOSXL.LITERAL_FORM.toString());
            delegate = new AnnotationValueShortFormProvider(
                    Collections.singletonList(literalForm),
                    createLangMap(literalForm, lang),
                    ontologies::stream,
                    defaultSFP);
        }
        else {
            delegate = new AnnotationValueShortFormProvider(
                    Collections.singletonList(annotProp),
                    createLangMap(annotProp, lang),
                    ontologies::stream,
                    defaultSFP);
        }
    }

    private boolean isSkosXLLabelAnnotation(OWLAnnotationProperty annotProp) {
        return annotProp.getIRI().toString().equals(SKOSXL.PREF_LABEL.toString());
    }

    @Nonnull
    public String getShortForm(@Nonnull OWLEntity owlEntity) {
        if (reifyRendering && isSkosConcept(owlEntity)) {
            Optional<OWLNamedIndividual> label = getLabelInstance(owlEntity.asOWLNamedIndividual());
            if (label.isPresent()) {
                return delegate.getShortForm(label.get());
            }
        }
        return delegate.getShortForm(owlEntity);
    }

    private Optional<OWLNamedIndividual> getLabelInstance(OWLNamedIndividual ind) {
        OWLAnnotationProperty prefLabel = df.getOWLAnnotationProperty(SKOSXL.PREF_LABEL.toString());
        for (OWLOntology o : ontologies) {
            for (OWLAnnotationAssertionAxiom annot : o.getAnnotationAssertionAxioms(ind.getIRI())) {
                if (annot.getProperty().equals(prefLabel)) {
                    Optional<IRI> value = annot.getValue().asIRI();
                    // TODO should check if there is an individual with this IRI
                    if (value.isPresent()) {
                        return Optional.of(df.getOWLNamedIndividual(value.get()));
                    }
                }
            }
        }
        return Optional.empty();
    }

    private boolean isSkosConcept(OWLEntity owlEntity) {
        if (owlEntity.isOWLNamedIndividual()) {
            OWLClass skosConcept = df.getOWLClass(SKOSVocabulary.CONCEPT.getIRI());
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

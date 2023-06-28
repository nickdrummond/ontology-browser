package org.coode.www.service;

import openllet.owlapi.OWL;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.io.StringWriter;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;


@Service
public class OWLAxiomService {

    private final Map<OWLOntology, Map<OWLAxiom, String>> axiomsRenderingsByOntology = new HashMap<>();

    public Set<OWLAxiom> findAxioms(final String search, final Set<OWLOntology> onts, final ShortFormProvider sfp) {
        return onts.stream().map(o -> findAxioms(search, o, sfp)).flatMap(Collection::stream).collect(Collectors.toSet());
    }

    public Set<OWLAxiom> findAxioms(final String search, final OWLOntology ont, final ShortFormProvider sfp) {
        return doIt(search, ont, sfp, e -> e.getValue().contains(search));
    }

    public Set<OWLAxiom> regexAxioms(final String search, final Set<OWLOntology> onts, final ShortFormProvider sfp) {
        return onts.stream().map(o -> regexAxioms(search, o, sfp)).flatMap(Collection::stream).collect(Collectors.toSet());
    }

    public Set<OWLAxiom> regexAxioms(final String search, final OWLOntology ont, final ShortFormProvider sfp) {
        return doIt(search, ont, sfp, e -> e.getValue().matches(search));
    }

    private Set<OWLAxiom> doIt(final String search, final OWLOntology ont, final ShortFormProvider sfp,
                               Predicate<? super Map.Entry<OWLAxiom, String>> filter) {
        ensureCache(ont, sfp);

        if (search == null || search.isEmpty()) {
            return axiomsRenderingsByOntology.get(ont).keySet();
        }
        else {
            return axiomsRenderingsByOntology.get(ont).entrySet().stream()
                    .filter(filter)
                    .map(Map.Entry::getKey)
                    .collect(Collectors.toSet());
        }
    }

    private void ensureCache(OWLOntology ont, ShortFormProvider sfp) {
        if (!axiomsRenderingsByOntology.containsKey(ont)) {
            axiomsRenderingsByOntology.put(ont, ont.getAxioms(Imports.EXCLUDED).stream().collect(Collectors.toMap(ax -> ax, ax -> render(ax, sfp))));
        }
    }

    private String render(final OWLAxiom axiom, final ShortFormProvider sfp) {
        StringWriter writer = new StringWriter();
        ManchesterOWLSyntaxObjectRenderer mosRenderer = new ManchesterOWLSyntaxObjectRenderer(writer, sfp);
        axiom.accept(mosRenderer);
        return writer.toString();
    }

}

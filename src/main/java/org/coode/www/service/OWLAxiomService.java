package org.coode.www.service;

import org.coode.www.model.Characteristic;
import org.coode.www.model.OWLObjectWithOntology;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.io.StringWriter;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;


@Service
public class OWLAxiomService {

    private final Map<OWLOntology, Map<OWLAxiom, String>> axiomsRenderingsByOntology = new HashMap<>();

    public Characteristic getAxioms(Set<OWLOntology> onts) {
        // avoid generating search indices as not needed
        List<OWLObjectWithOntology> results = onts.stream()
                .flatMap(o -> wrappedWithOntology(o.axioms(Imports.EXCLUDED), o))
                .collect(Collectors.toList());
        return new Characteristic(null, "Axioms", results);
    }

    public Characteristic findAxioms(final String search, final Set<OWLOntology> onts, final ShortFormProvider sfp) {
        return resultsCharacteristic(search, onts, sfp, e -> e.getValue().contains(search));
    }

    public Characteristic regexAxioms(final String search, final Set<OWLOntology> onts, final ShortFormProvider sfp) {
        return resultsCharacteristic(search, onts, sfp, e -> e.getValue().matches(search));
    }

    private Characteristic resultsCharacteristic(final String search,
                                                 final Set<OWLOntology> onts,
                                                 final ShortFormProvider sfp,
                                                 final Predicate<? super Map.Entry<OWLAxiom, String>> filter) {
        List<OWLObjectWithOntology> results = onts.stream()
                .flatMap(o -> wrappedWithOntology(filterAxioms(search, o, sfp, filter), o))
                .collect(Collectors.toList());
        return new Characteristic(null, "Axioms containing \"" + search + "\"", results);
    }

    private Stream<OWLAxiom> filterAxioms(final String search,
                                          final OWLOntology ont,
                                          final ShortFormProvider sfp,
                                          final Predicate<? super Map.Entry<OWLAxiom, String>> filter) {
        ensureCache(ont, sfp);

        if (search == null || search.isEmpty()) {
            return axiomsRenderingsByOntology.get(ont).keySet().stream();
        }
        else {
            return axiomsRenderingsByOntology.get(ont).entrySet().stream()
                    .filter(filter)
                    .map(Map.Entry::getKey);
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

    private Stream<OWLObjectWithOntology> wrappedWithOntology(Stream<OWLAxiom> axioms, OWLOntology ont) {
        return axioms.map(a -> new OWLObjectWithOntology(a, ont));
    }
}

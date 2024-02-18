package org.coode.www.service;

import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.paging.PageData;
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

    public Characteristic getAxioms(
            Set<OWLOntology> onts,
            int start,
            int pageSize) {

        // avoid generating search indices as not needed
        List<AxiomWithMetadata> results = onts.stream()
                .flatMap(o -> wrappedWithOntology(o.axioms(Imports.EXCLUDED), o))
                .toList();

        List<AxiomWithMetadata> paged = results.stream().skip(start-1L).limit(pageSize).toList();

        return new Characteristic(null, "Axioms",
                paged,
                new PageData(start, paged.size(), results.size()));
    }

    public Characteristic findAxioms(
            final String search,
            final Set<OWLOntology> onts,
            final ShortFormProvider sfp,
            int start,
            int pageSize) {

        Predicate<Map.Entry<OWLAxiom, String>> containsIgnoreCase = e ->
                e.getValue().toLowerCase().contains(search.toLowerCase());

        return resultsCharacteristic(search, onts, sfp, containsIgnoreCase, start, pageSize);
    }

    private Characteristic resultsCharacteristic(
            final String search,
            final Set<OWLOntology> onts,
            final ShortFormProvider sfp,
            final Predicate<? super Map.Entry<OWLAxiom, String>> filter,
            int start,
            int pageSize) {
        List<AxiomWithMetadata> results = onts.stream()
                .flatMap(o -> wrappedWithOntology(filterAxioms(search, o, sfp, filter), o))
                .toList();
        if (start > results.size()) {
            throw new RuntimeException("Start out of range");
        }
        List<AxiomWithMetadata> paged = results.stream().skip(start-1L).limit(pageSize).toList();
        return new Characteristic(null, "Axioms containing \"" + search + "\"",
                paged,
                new PageData(start, paged.size(), results.size()));
    }

    private Stream<OWLAxiom> filterAxioms(
            final String search,
            final OWLOntology ont,
            final ShortFormProvider sfp,
            final Predicate<? super Map.Entry<OWLAxiom, String>> filter) {
        ensureCache(ont, sfp);

        if (search == null || search.isEmpty()) {
            return axiomsRenderingsByOntology.get(ont).keySet().stream();
        } else {
            return axiomsRenderingsByOntology.get(ont).entrySet().stream()
                    .filter(filter)
                    .map(Map.Entry::getKey);
        }
    }

    private void ensureCache(OWLOntology ont, ShortFormProvider sfp) {
        axiomsRenderingsByOntology.putIfAbsent(ont,
                ont.getAxioms(Imports.EXCLUDED).stream()
                        .collect(Collectors.toMap(ax -> ax, ax -> render(ax, sfp))));
    }

    private String render(final OWLAxiom axiom, final ShortFormProvider sfp) {
        StringWriter writer = new StringWriter();
        ManchesterOWLSyntaxObjectRenderer mosRenderer = new ManchesterOWLSyntaxObjectRenderer(writer, sfp);
        axiom.accept(mosRenderer);
        return writer.toString();
    }

    private Stream<AxiomWithMetadata> wrappedWithOntology(Stream<OWLAxiom> axioms, OWLOntology ont) {
        return axioms.map(a -> new AxiomWithMetadata("Axioms", a, a, ont));
    }
}

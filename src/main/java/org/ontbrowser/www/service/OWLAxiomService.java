package org.ontbrowser.www.service;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.RestartListener;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.PageData;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class OWLAxiomService implements RestartListener {

    private static final Logger log = LoggerFactory.getLogger(OWLAxiomService.class);

    private final Map<OWLOntology, Map<OWLAxiom, String>> axiomsRenderingsByOntology = new HashMap<>();

    public OWLAxiomService(@Autowired OWLHTMLKit kit) {
        kit.registerListener(this);
    }

    public Characteristic getAxioms(
            OWLOntology ont,
            Imports imports,
            int start,
            int pageSize) {
        Function<OWLOntology, Stream<? extends OWLAxiom>> getAxiomsForOntology = (OWLOntology o) -> o.axioms(Imports.EXCLUDED);
        return getAxioms(ont, imports, start, pageSize, getAxiomsForOntology);
    }

    public Characteristic getAxiomsOfType(
            OWLOntology ont,
            Imports imports,
            int start,
            int pageSize,
            @Nonnull AxiomType<OWLAxiom> axiomType) {
        Function<OWLOntology, Stream<? extends OWLAxiom>> getAxiomsForOntology = (OWLOntology o) -> o.axioms(axiomType, Imports.EXCLUDED);
        return getAxioms(ont, imports, start, pageSize, getAxiomsForOntology);
    }

    public Characteristic getLogicalAxioms(
            OWLOntology ont,
            Imports imports,
            int start,
            int pageSize) {
        Function<OWLOntology, Stream<? extends OWLAxiom>> getAxiomsForOntology = (OWLOntology o) -> o.logicalAxioms(Imports.EXCLUDED);
        return getAxioms(ont, imports, start, pageSize, getAxiomsForOntology);
    }

    private Characteristic getAxioms(
            OWLOntology ont,
            Imports imports,
            int start,
            int pageSize,
            Function<OWLOntology, Stream<? extends OWLAxiom>> getAxiomsForOntology) {
        Stream<OWLOntology> ontologies = imports == Imports.INCLUDED ? ont.importsClosure() : Stream.of(ont);
        List<AxiomWithMetadata> results = ontologies
                .flatMap(o -> wrappedWithOntology(getAxiomsForOntology.apply(o), o))
                .toList();

        List<AxiomWithMetadata> paged = results.stream().skip(start - 1L).limit(pageSize).toList();

        return new Characteristic(null, "Axioms",
                paged,
                new PageData(start, paged.size(), results.size()));

    }

    public Characteristic findAxioms(
            final String search,
            final OWLOntology ont,
            final ShortFormProvider sfp,
            int start,
            int pageSize) {

        String stripped = normalise(search);

        Predicate<Map.Entry<OWLAxiom, String>> contains = entry ->
                entry.getValue().contains(stripped);

        return resultsCharacteristic(search, ont, sfp, contains, start, pageSize);
    }

    public Characteristic findLogicalAxioms(
            final String search,
            final OWLOntology ont,
            final ShortFormProvider sfp,
            int start,
            int pageSize) {

        String stripped = normalise(search);

        Predicate<Map.Entry<OWLAxiom, String>> contains = entry ->
                entry.getKey().isLogicalAxiom() && entry.getValue().contains(stripped);

        return resultsCharacteristic(search, ont, sfp, contains, start, pageSize);
    }

    public Characteristic findAxiomsByType(
            @Nonnull final String search,
            final OWLOntology ont,
            final ShortFormProvider sfp,
            int start,
            int pageSize,
            @Nonnull final AxiomType<? extends OWLAxiom> entityType) {

        String stripped = normalise(search);

        Predicate<Map.Entry<OWLAxiom, String>> contains = entry ->
                entry.getKey().getAxiomType().equals(entityType) && entry.getValue().contains(stripped);

        return resultsCharacteristic(search, ont, sfp, contains, start, pageSize);
    }

    private Characteristic resultsCharacteristic(
            final String search,
            final OWLOntology ont,
            final ShortFormProvider sfp,
            final Predicate<? super Map.Entry<OWLAxiom, String>> filter,
            int start,
            int pageSize) {

        List<AxiomWithMetadata> results = ont.importsClosure()
                .flatMap(o -> wrappedWithOntology(filterAxioms(search, o, sfp, filter), o))
                .toList();

        int size = results.size();

        if (start > size) {
            start = 1;
        }

        List<AxiomWithMetadata> paged = results.stream().skip(start - 1L).limit(pageSize).toList();

        return new Characteristic(null, "Axioms containing \"" + search + "\"",
                paged,
                size == 0 ? new PageData(1, 0, 0) : new PageData(start, pageSize, size));
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
        axiomsRenderingsByOntology.computeIfAbsent(ont, key -> cacheAxiomRenderings(key, sfp));
    }

    private Map<OWLAxiom, String> cacheAxiomRenderings(OWLOntology ont, ShortFormProvider sfp) {
        log.info("Building axiom cache for {}", ont.getOntologyID());
        return ont.getAxioms(Imports.EXCLUDED).stream()
                .collect(Collectors.toMap(ax -> ax, ax -> render(ax, sfp)));
    }

    private String render(final OWLAxiom axiom, final ShortFormProvider sfp) {
        StringWriter writer = new StringWriter();
        var mosRenderer = new ManchesterOWLSyntaxObjectRenderer(writer, sfp);
        axiom.accept(mosRenderer);
        return normalise(writer.toString());
    }

    private String normalise(String s) {
        return s.replace("\n", "")
                .replace("(", "")
                .replace(")", "")
                .toLowerCase();
    }

    private Stream<AxiomWithMetadata> wrappedWithOntology(Stream<? extends OWLAxiom> axioms, OWLOntology ont) {
        return axioms.map(a -> new AxiomWithMetadata("Axioms", a, a, ont));
    }

    @Override
    public void onRestart() {
        log.info("Clearing axiom cache");
        axiomsRenderingsByOntology.clear();
    }
}

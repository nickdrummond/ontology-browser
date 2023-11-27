package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

public class OntologyCharacteristicsBuilder {

    private final List<Characteristic> characteristics;

    public OntologyCharacteristicsBuilder(OWLOntology ont, Comparator<OWLObject> comparator) {
        characteristics = new ArrayList<>();

        for (Optional<Characteristic> c : asList(
                getAnnotations(ont, comparator),
                getImports(ont, comparator),
                getGeneralClassAxioms(ont, comparator)
        )) {
            c.ifPresent(characteristics::add);
        }
    }

    public List<Characteristic> getCharacteristics() {
        return characteristics;
    }

    public Optional<Characteristic> getAnnotations(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("Annotations", ont,
                wrapWithOntology(ont.getAnnotations(), ont, c));
    }

    public Optional<Characteristic> getImports(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("Imports", ont,
                wrapWithOntology(ont.getDirectImportsDocuments(), ont, c));
    }

    public Optional<Characteristic> getGeneralClassAxioms(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("General Class Axioms", ont,
                wrapClassAxioms(ont.getGeneralClassAxioms(), ont, c));
    }

    private List<AxiomWithMetadata> wrapWithOntology(Set<? extends OWLObject> objs, OWLOntology ont, Comparator<OWLObject> c) {
        return objs.stream()
                .sorted(c)
                .map(o -> new AxiomWithMetadata("s", o, null,  ont))
                .collect(Collectors.toList());
    }

    private List<AxiomWithMetadata> wrapClassAxioms(Set<OWLClassAxiom> axioms, OWLOntology ont, Comparator<OWLObject> c) {
        return axioms.stream()
                .sorted(c)
                .map(ax -> new AxiomWithMetadata("s", ax, ax,  ont))
                .collect(Collectors.toList());
    }

    private Optional<Characteristic> asCharacteristicNew(String name, OWLObject owlObject, List<AxiomWithMetadata> results) {
        return results.isEmpty() ? Optional.empty() : Optional.of(new Characteristic(owlObject, name, results));
    }
}

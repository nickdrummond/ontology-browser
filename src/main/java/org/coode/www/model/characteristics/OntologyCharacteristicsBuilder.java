package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.paging.With;
import org.coode.www.util.PagingUtils;
import org.semanticweb.owlapi.model.*;

import java.util.*;

import static java.util.Arrays.asList;

public class OntologyCharacteristicsBuilder {

    private final List<Characteristic> characteristics = new ArrayList<>();

    public OntologyCharacteristicsBuilder(
            final OWLOntology ont,
            final List<With> with,
            final int defaultPageSize,
            final Comparator<OWLObject> comparator) {

        for (Optional<Characteristic> c : asList(
                createCharacteristic("Annotations", ont.getAnnotations(), ont, comparator, with, defaultPageSize),
                createCharacteristic("Imports", ont.getDirectImportsDocuments(), ont, comparator, with, defaultPageSize),
                createCharacteristic("General Class Axioms", ont.getGeneralClassAxioms(), ont, comparator, with, defaultPageSize)
        )) {
            c.ifPresent(characteristics::add);
        }
    }

    private Optional<Characteristic> createCharacteristic(
            final String name,
            final Set<? extends OWLObject> objs,
            final OWLOntology ont,
            final Comparator<OWLObject> c,
            final List<With> with,
            final int defaultPageSize) {

        if (objs.isEmpty()) {
            return Optional.empty();
        }

        List<AxiomWithMetadata> r = objs.stream()
                .map(o -> new AxiomWithMetadata("s", o, null, ont))
                .toList();

        Comparator<AxiomWithMetadata> compareByOWLObject = (a, b) ->
                c.compare(a.getOWLObject(), b.getOWLObject());

        return Optional.of(PagingUtils.getCharacteristic(ont, with, defaultPageSize, compareByOWLObject, name, r));
    }

    public List<Characteristic> getCharacteristics() {
        return characteristics;
    }
}

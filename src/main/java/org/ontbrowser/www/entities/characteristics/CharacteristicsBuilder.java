package org.ontbrowser.www.entities.characteristics;

import com.google.common.collect.Streams;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.util.PagingUtils;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public abstract class CharacteristicsBuilder<T extends OWLEntity> {

    public static final String USAGE = "Usage";
    public static final String ANNOTATIONS = "Annotations";

    private final List<Characteristic> characteristics;

    protected CharacteristicsBuilder(
            T target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int defaultPageSize) {
        // TODO don't do all the work in the constructor
        Stream<InterestingFilter> filters = ont.getImportsClosure().stream()
                .map(o -> createFilter(o, target));

        Stream<AxiomWithMetadata> axiomsWithMetadata = filters
                .flatMap(InterestingFilter::findAxioms);

        Map<String, List<AxiomWithMetadata>> sortAndGroupByType = axiomsWithMetadata
                .collect(groupingBy(AxiomWithMetadata::getType));

        Comparator<AxiomWithMetadata> compareByOWLObject = (a, b) ->
                comparator.compare(a.getOWLObject(), b.getOWLObject());

        Comparator<Characteristic> compareCharacteristics = (a, b) ->
                getOrder().indexOf(a.getName()) - getOrder().indexOf(b.getName());

        characteristics = sortAndGroupByType.entrySet().stream()
                .map(entry -> PagingUtils.getCharacteristic(
                        target, with, defaultPageSize, compareByOWLObject, entry.getKey(), entry.getValue()
                ))
                .sorted(compareCharacteristics) // Sort by "order"
                .toList();
    }

    public List<Characteristic> getCharacteristics() {
        return characteristics;
    }

    // TODO lazy retrieve one characteristic?
    public Optional<Characteristic> getCharacteristic(String name) {
        return characteristics.stream().filter(ch -> Objects.equals(ch.getName(), name)).findFirst();
    }

    abstract InterestingFilter createFilter(OWLOntology ont, T target);

    abstract List<String> getOrder();


    public abstract class InterestingFilter implements OWLAxiomVisitorEx<AxiomWithMetadata> {
        protected final OWLOntology ont;
        protected final T target;

        protected InterestingFilter(OWLOntology ont, T target) {
            this.ont = ont;
            this.target = target;
        }

        protected Stream<AxiomWithMetadata> findAxioms() {
            return Streams.concat(
                    ont.annotationAssertionAxioms(target.getIRI(), Imports.EXCLUDED),
                    ont.referencingAxioms(target, Imports.EXCLUDED)
            ).map(ax -> ax.accept(this));
        }

        @Override
        public <U> AxiomWithMetadata doDefault(U axiom) {
            // TODO types
            return new AxiomWithMetadata(USAGE, (OWLObject) axiom, (OWLAxiom) axiom, ont);
        }

        @Override
        public AxiomWithMetadata visit(OWLAnnotationAssertionAxiom axiom) {
            return wrap(ANNOTATIONS, axiom,
                    axiom.getSubject().equals(target.getIRI()),
                    axiom::getAnnotation);
        }

        public OWLOntology getOntology() {
            return ont;
        }

        protected AxiomWithMetadata wrap(String name,
                                         OWLAxiom axiom,
                                         boolean condition,
                                         Supplier<OWLObject> getObject) {
            if (condition) {
                return new AxiomWithMetadata(name, getObject.get(), axiom, ont);
            }
            return new AxiomWithMetadata(USAGE, axiom, axiom, ont);
        }
    }
}

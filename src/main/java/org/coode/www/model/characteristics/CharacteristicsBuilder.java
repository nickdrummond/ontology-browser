package org.coode.www.model.characteristics;

import com.google.common.collect.Streams;
import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public abstract class CharacteristicsBuilder<T extends OWLEntity> {

    public static final String USAGE = "Usage";
    public static final String ANNOTATIONS = "Annotations";

    private final List<Characteristic> characteristics;

    protected CharacteristicsBuilder(T target,
                                  Set<OWLOntology> activeOntologies,
                                  Comparator<OWLObject> comparator) {

        Stream<InterestingFilter> filters = activeOntologies.stream()
                .map(o -> createFilter(o, target));

        Stream<AxiomWithMetadata> axiomsWithMetadata = filters
                .flatMap(InterestingFilter::findAxioms);

        Comparator<AxiomWithMetadata> compareByOWLObject = (a, b) ->
                comparator.compare(a.getOWLObject(), b.getOWLObject());

        Map<String, List<AxiomWithMetadata>> sortAndGroupByType = axiomsWithMetadata
                .sorted(compareByOWLObject)
                .collect(groupingBy(AxiomWithMetadata::getType));

        characteristics = sortAndGroupByType.entrySet().stream()
                .map(entry -> new Characteristic(target, entry.getKey(), entry.getValue()))
                .sorted((a, b) -> getOrder().indexOf(a.getName()) - getOrder().indexOf(b.getName())) // Sort by "order"
                .collect(Collectors.toList());
    }

    public List<Characteristic> getCharacteristics() {
        return characteristics;
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

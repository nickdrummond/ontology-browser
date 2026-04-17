package org.ontbrowser.www.feature.entities.characteristics;

import com.google.common.collect.Streams;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public abstract class CharacteristicsBuilder<T extends OWLEntity> {

    public static final String USAGE = "Usage";
    public static final String ANNOTATIONS = "Annotations";

    private final T target;
    private final OWLOntology ont;
    private final Comparator<OWLObject> comparator;
    private final List<With> with;
    private final int defaultPageSize;
    private OWLObject focus = null;

    protected CharacteristicsBuilder(
            T target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int defaultPageSize) {
        this.target = target;
        this.ont = ont;
        this.comparator = comparator;
        this.with = with;
        this.defaultPageSize = defaultPageSize;
    }

    public CharacteristicsBuilder<T> withFocus(OWLObject focus) {
        this.focus = focus;
        return this;
    }

    public List<Characteristic> getCharacteristics() {
        Stream<AxiomWithMetadata> axiomsWithMetadata = getAxioms();

        Map<String, List<AxiomWithMetadata>> sortAndGroupByType = axiomsWithMetadata
                .collect(groupingBy(AxiomWithMetadata::type));

        return sortAndGroupByType.entrySet().stream()
                .map(entry -> createCharacteristic(entry.getKey(), entry.getValue()))
                .sorted((a, b) ->
                        getOrder().indexOf(a.getName()) - getOrder().indexOf(b.getName())) // Sort by "order"
                .toList();
    }

    public Optional<Characteristic> getCharacteristic(String name) {
        var filtered = getAxioms()
                .filter(axiomWithMetadata -> axiomWithMetadata.type().equals(name))
                .toList();
        return filtered.isEmpty() ? Optional.empty() : Optional.of(createCharacteristic(name, filtered));
    }

    private Characteristic createCharacteristic(String name, List<AxiomWithMetadata> values) {
        return PagingUtils.getCharacteristic(
                target, with, defaultPageSize,
                (a, b) -> comparator.compare(a.owlObject(), b.owlObject()),
                name, values, focus);
    }

    private Stream<AxiomWithMetadata> getAxioms() {
        return createFilter(ont, target).findAxioms(Imports.INCLUDED);
    }

    abstract InterestingFilter createFilter(OWLOntology ont, T target);

    abstract List<String> getOrder();


    // TODO change so that this uses the imports closure
    public abstract class InterestingFilter implements OWLAxiomVisitorEx<AxiomWithMetadata> {
        protected final OWLOntology ont;
        protected final T target;

        protected InterestingFilter(OWLOntology ont, T target) {
            this.ont = ont;
            this.target = target;
        }

        protected Stream<AxiomWithMetadata> findAxioms(Imports imports) {
            return Streams.concat(
                    ont.annotationAssertionAxioms(target.getIRI(), imports),
                    ont.referencingAxioms(target, imports)
            ).map(ax -> ax.accept(this));
        }

        @Override
        public <U> AxiomWithMetadata doDefault(U axiom) {
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

package org.ontbrowser.www.feature.graph;

import org.apache.commons.lang3.NotImplementedException;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

// TODO could make the properties a list and use that order for display?
public class GraphDescriptor {

    private static final Logger log = LoggerFactory.getLogger(GraphDescriptor.class);

    private final OWLOntology ont;
    private final OWLDataFactory df;

    private final Set<OWLProperty> withProperties = new HashSet<>();
    private final Set<OWLProperty> followProperties = new HashSet<>();

    private final Set<OWLProperty> withInverseProperties = new HashSet<>();
    private final Set<OWLProperty> withoutProperties = new HashSet<>();
    private final Set<OWLNamedIndividual> individuals = new HashSet<>();
    private final Set<OWLClass> classes = new HashSet<>();
    private int depth = 1;
    private String name = "anon";

    public GraphDescriptor(OWLOntology ont) {
        this.ont = ont;
        this.df = ont.getOWLOntologyManager().getOWLDataFactory();
    }

    public GraphDescriptor withName(String name) {
        this.name = name;
        return this;
    }

    public GraphDescriptor withProperty(OWLProperty property) {
        this.withProperties.add(property);
        return this;
    }

    public GraphDescriptor withProperties(OWLProperty... properties) {
        this.withProperties.addAll(Arrays.asList(properties));
        return this;
    }

    public GraphDescriptor withProperties(Set<? extends OWLProperty> properties) {
        this.withProperties.addAll(properties);
        return this;
    }

    public GraphDescriptor withInverseProperties(Set<OWLProperty> invProps) {
        this.withInverseProperties.addAll(invProps);
        return this;
    }

    public GraphDescriptor withoutProperty(OWLProperty property) {
        this.withoutProperties.add(property);
        return this;
    }

    public GraphDescriptor withoutProperties(OWLProperty... properties) {
        this.withoutProperties.addAll(Arrays.asList(properties));
        return this;
    }

    public GraphDescriptor withoutProperties(GraphDescriptor... others) {
        Arrays.asList(others).forEach(descriptor -> this.withoutProperties.addAll(descriptor.getProperties()));
        return this;
    }

    public GraphDescriptor withFollow(Set<OWLProperty> followProperties) {
        this.withProperties.addAll(followProperties);
        this.followProperties.addAll(followProperties);
        return this;
    }

    private Set<OWLProperty> getProperties() {
        return withProperties;
    }

    public GraphDescriptor addEntity(OWLEntity entity) {
        if (entity instanceof OWLNamedIndividual ind) {
            return addIndividual(ind);
        }
        if (entity instanceof OWLClass cls) {
            return addClass(cls);
        }
        throw new NotImplementedException("Unsupported graph type " + entity.getClass());
    }

    public GraphDescriptor addEntities(Set<OWLNamedIndividual> individuals) {
        this.individuals.addAll(individuals);
        return this;
    }

    public GraphDescriptor addIndividual(OWLNamedIndividual ind) {
        this.individuals.add(ind);
        return this;
    }

    public GraphDescriptor addClass(OWLClass cls) {
        this.classes.add(cls);
        return this;
    }

    public GraphDescriptor withDepth(int depth) {
        this.depth = depth;
        return this;
    }

    boolean isAllowedProperty(OWLProperty property) {
        return withProperties.contains(property) && !withoutProperties.contains(property);
    }

    boolean isAllowedInverseProperty(OWLProperty property) {
        return withInverseProperties.contains(property) && !withoutProperties.contains(property);
    }

    boolean isFollowProperty(OWLProperty property) {
        return followProperties.contains(property);
    }

    public Set<OWLNamedIndividual> getIndividuals() {
        return individuals;
    }

    public Set<OWLClass> getClasses() {
        return classes;
    }

    public int getMaxDepth() {
        return depth;
    }

    public OWLOntology getOntology() {
        return ont;
    }
}

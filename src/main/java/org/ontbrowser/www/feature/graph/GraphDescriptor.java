package org.ontbrowser.www.feature.graph;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;

import java.util.HashSet;
import java.util.Set;

// TODO could make the properties a list and use that order for display?
public class GraphDescriptor {

    private final OWLOntology ont;

    private final Set<OWLProperty> withProperties = new HashSet<>();
    private final Set<OWLProperty> followProperties = new HashSet<>();

    private final Set<OWLProperty> withInverseProperties = new HashSet<>();
    private final Set<OWLProperty> withoutProperties = new HashSet<>();
    private final Set<OWLObject> owlObjects = new HashSet<>();

    private int depth = 1;

    public GraphDescriptor(OWLOntology ont) {
        this.ont = ont;
    }

    public GraphDescriptor withProperties(Set<? extends OWLProperty> properties) {
        this.withProperties.addAll(properties);
        return this;
    }

    public GraphDescriptor withInverseProperties(Set<OWLProperty> invProps) {
        this.withInverseProperties.addAll(invProps);
        return this;
    }


    public GraphDescriptor withoutProperties(Set<OWLProperty> properties) {
        this.withoutProperties.addAll(properties);
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

    public GraphDescriptor addObject(OWLClass cls) {
        this.owlObjects.add(cls);
        return this;
    }

    public GraphDescriptor addObjects(Set<? extends OWLObject> owlObjects) {
        this.owlObjects.addAll(owlObjects);
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

    public Set<OWLObject> getOwlObjects() {
        return owlObjects;
    }

    public int getMaxDepth() {
        return depth;
    }

    public OWLOntology getOntology() {
        return ont;
    }
}

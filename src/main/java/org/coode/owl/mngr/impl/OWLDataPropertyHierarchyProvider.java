/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public class OWLDataPropertyHierarchyProvider implements HierarchyProvider<OWLDataProperty> {

    private OWLServer server;

    private Set<OWLDataProperty> implicitRoots;

    private ActiveOntologyProvider.Listener serverListener = new ActiveOntologyProvider.Listener(){
        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };

    private OWLOntologyChangeListener ontologyListener = new OWLOntologyChangeListener(){

        public void ontologiesChanged(List<? extends OWLOntologyChange> changes) throws OWLException {
            for (OWLOntologyChange change : changes){
                if (change.isAxiomChange()){
                    if (change.getAxiom() instanceof OWLSubPropertyAxiom){
                        reset();
                        return;
                    }
                }
            }
        }
    };


    public OWLDataPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addActiveOntologyListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }

    private OWLDataProperty getTopProperty() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty();
    }

    private Set<OWLDataProperty> getPropertiesInSignature(OWLOntology ont){
        return ont.getDataPropertiesInSignature();
    }

    protected OWLServer getServer() {
        return server;
    }

    @Override
    public Class<? extends OWLDataProperty> getNodeClass() {
        return OWLDataProperty.class;
    }

    public Set<OWLDataProperty> getRoots() {
        return Collections.singleton(getTopProperty());
    }

    public boolean isRoot(OWLDataProperty node) {
        return node.equals(getTopProperty());
    }

    public boolean isLeaf(OWLDataProperty node) {
        return getChildren(node).isEmpty();
    }

    public boolean hasAncestor(OWLDataProperty node, OWLDataProperty ancestor) {
        return getAncestors(node).contains(ancestor);
    }

    public Set<OWLDataProperty> getParents(OWLDataProperty node) {
        Set<OWLDataProperty> supers;
        if (isRoot(node)){
            supers = Collections.emptySet();
        }
        else{
            supers = new HashSet<OWLDataProperty>();
            for (OWLDataPropertyExpression pe :  EntitySearcher.getSuperProperties(node, getOntologies())){
                if (!pe.isAnonymous()) {
                    supers.add(pe.asOWLDataProperty());
                }
            }
            if (supers.isEmpty()){
                supers.addAll(getRoots());
            }
        }
        return supers;
    }

    public Set<OWLDataProperty> getChildren(OWLDataProperty node) {
        if (isRoot(node)){
            return getImplicitRoots();
        }

        Set<OWLDataProperty> subs = new HashSet<OWLDataProperty>();

        for (OWLDataPropertyExpression pe :  EntitySearcher.getSubProperties(node, getOntologies())){
            if (!pe.isAnonymous()) {
                subs.add(pe.asOWLDataProperty());
            }
        }

        return subs;
    }

    public Set<OWLDataProperty> getEquivalents(OWLDataProperty node) {
        Set<OWLDataProperty> equivs = new HashSet<OWLDataProperty>();
        for (OWLDataPropertyExpression pe :  EntitySearcher.getEquivalentProperties(node, getOntologies())){
            if (!pe.isAnonymous()) {
                equivs.add(pe.asOWLDataProperty());
            }
        }
        return equivs;
    }


    public Set<OWLDataProperty> getDescendants(OWLDataProperty node) {
        if (isRoot(node)){
            return getAllReferencedProperties();
        }

        Set<OWLDataProperty> descendants = new HashSet<OWLDataProperty>();
        List<OWLDataProperty> nodes = new ArrayList<OWLDataProperty>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (OWLDataProperty child : getChildren(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        descendants.addAll(nodes);
        descendants.remove(node);
        return descendants;
    }

    public Set<OWLDataProperty> getAncestors(OWLDataProperty node) {
        if (isRoot(node)){
            return Collections.emptySet();
        }

        Set<OWLDataProperty> ancestors;
        List<OWLDataProperty> nodes = new ArrayList<OWLDataProperty>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (OWLDataProperty child : getParents(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        if (nodes.size() == 1){
            ancestors = getRoots();
        }
        else{
            ancestors = new HashSet<OWLDataProperty>(nodes);
            ancestors.remove(node);
        }
        return ancestors;
    }

    protected Set<OWLOntology> getOntologies() {
        return server.getActiveOntologies();
    }

    public void dispose() {
        server.getOWLOntologyManager().removeOntologyChangeListener(ontologyListener);
        server.removeActiveOntologyListener(serverListener);
        server = null;
    }

    private void reset() {
        implicitRoots = null;
    }

    private Set<OWLDataProperty> getAllReferencedProperties() {
        Set<OWLDataProperty> props = new HashSet<OWLDataProperty>();
        for (OWLOntology ont : getOntologies()){
            props.addAll(getPropertiesInSignature(ont));
        }
        return props;
    }

    private Set<OWLDataProperty> getImplicitRoots() {
        if (implicitRoots == null){
            implicitRoots = getAllReferencedProperties();
            implicitRoots.removeAll(getRoots());
            for (Iterator<OWLDataProperty> i=implicitRoots.iterator(); i.hasNext();){
                OWLDataProperty p = i.next();
                final Iterable<OWLDataPropertyExpression> supers = EntitySearcher.getSuperProperties(p, getOntologies());
                if (!supers.iterator().hasNext()){
                    // do nothing
                }
                else if (supers.equals(getRoots())){
                    // do nothing
                }
                else{
                    i.remove();
                }
            }
        }
        return implicitRoots;
    }
}
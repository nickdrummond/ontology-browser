/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.*;

import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 23, 2008<br><br>
 */
public abstract class AbstractPropertyHierarchyProvider<P extends OWLProperty> implements HierarchyProvider<P> {

    private OWLServer server;

    private Set<P> implicitRoots;

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


    public AbstractPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addActiveOntologyListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }

    protected abstract P getTopProperty();

    protected abstract Set<P> getPropertiesInSignature(OWLOntology ont);

    protected OWLServer getServer() {
        return server;
    }

    public Set<P> getRoots() {
        return Collections.singleton(getTopProperty());
    }

    public boolean isRoot(P node) {
        return node.equals(getTopProperty());
    }

    public boolean isLeaf(P node) {
        return getChildren(node).isEmpty();
    }

    public boolean hasAncestor(P node, P ancestor) {
        return getAncestors(node).contains(ancestor);
    }

    public Set<P> getParents(P node) {
        Set<P> supers;
        if (isRoot(node)){
            supers = Collections.emptySet();
        }
        else{
            supers = new HashSet<P>();
            for (Object pe : node.getSuperProperties(getOntologies())){
                if (pe instanceof OWLProperty){
                    supers.add((P)pe); // it must be a P
                }
            }
            if (supers.isEmpty()){
                supers.addAll(getRoots());
            }
        }
        return supers;
    }


    public Set<P> getChildren(P node) {
        if (isRoot(node)){
            return getImplicitRoots();
        }

        Set<P> subs = new HashSet<P>();

        for (Object pe : node.getSubProperties(getOntologies())){
                subs.add((P)pe);
        }

        return subs;
    }


    public Set<P> getEquivalents(P node) {
        Set<P> equivs = new HashSet<P>();
        for (Object pe : node.getEquivalentProperties(getOntologies())){
                equivs.add((P)pe);
        }
        return equivs;
    }


    public Set<P> getDescendants(P node) {
        if (isRoot(node)){
            return getAllReferencedProperties();
        }

        Set<P> descendants = new HashSet<P>();
        List<P> nodes = new ArrayList<P>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (P child : getChildren(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        descendants.addAll(nodes);
        descendants.remove(node);
        return descendants;
    }

    public Set<P> getAncestors(P node) {
        if (isRoot(node)){
            return Collections.emptySet();
        }

        Set<P> ancestors;
        List<P> nodes = new ArrayList<P>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (P child : getParents(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        if (nodes.size() == 1){
            ancestors = getRoots();
        }
        else{
            ancestors = new HashSet<P>(nodes);
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

    private Set<P> getAllReferencedProperties() {
        Set<P> props = new HashSet<P>();
        for (OWLOntology ont : getOntologies()){
            props.addAll(getPropertiesInSignature(ont));
        }
        return props;
    }

    private Set<P> getImplicitRoots() {
        if (implicitRoots == null){
            implicitRoots = getAllReferencedProperties();
            implicitRoots.removeAll(getRoots());
            for (Iterator i=implicitRoots.iterator(); i.hasNext();){
                P p = (P)i.next();
                final Set supers = p.getSuperProperties(getOntologies());
                if (supers.isEmpty()){
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
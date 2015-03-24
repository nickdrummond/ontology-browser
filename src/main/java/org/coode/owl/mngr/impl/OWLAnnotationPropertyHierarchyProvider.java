package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.*;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 6, 2010<br><br>
 */
public class OWLAnnotationPropertyHierarchyProvider implements HierarchyProvider<OWLAnnotationProperty> {

   private OWLServer server;

    private Set<OWLAnnotationProperty> implicitRoots;

    private ActiveOntologyProvider.Listener serverListener = new ActiveOntologyProvider.Listener(){
        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };

    private OWLOntologyChangeListener ontologyListener = new OWLOntologyChangeListener(){

        public void ontologiesChanged(List<? extends OWLOntologyChange> changes) throws OWLException {
            for (OWLOntologyChange change : changes){
                if (change.isAxiomChange()){
                    if (change.getAxiom() instanceof OWLAnnotationAssertionAxiom){
                        reset();
                        return;
                    }
                }
            }
        }
    };


    public OWLAnnotationPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addActiveOntologyListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }


    public Class<? extends OWLAnnotationProperty> getNodeClass() {
        return OWLAnnotationProperty.class;
    }

    public Set<OWLAnnotationProperty> getRoots() {
        if (implicitRoots == null){
            implicitRoots = new HashSet<OWLAnnotationProperty>();
            for (OWLOntology ont : getOntologies()){
                implicitRoots.addAll(ont.getAnnotationPropertiesInSignature());
            }
        }
        return implicitRoots;
    }

    public boolean isRoot(OWLAnnotationProperty node) {
        return true;
    }

    public boolean isLeaf(OWLAnnotationProperty node) {
        return true;
    }


    public Set<OWLAnnotationProperty> getParents(OWLAnnotationProperty node) {
        return Collections.emptySet();
    }


    public Set<OWLAnnotationProperty> getChildren(OWLAnnotationProperty node) {
        return Collections.emptySet();
    }


    public Set<OWLAnnotationProperty> getEquivalents(OWLAnnotationProperty node) {
        return Collections.emptySet();
    }


    public Set<OWLAnnotationProperty> getDescendants(OWLAnnotationProperty node) {
        return Collections.emptySet();
    }


    public Set<OWLAnnotationProperty> getAncestors(OWLAnnotationProperty node) {
        return Collections.emptySet();
    }

    public boolean hasAncestor(OWLAnnotationProperty node, OWLAnnotationProperty ancestor) {
        return false;
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
}

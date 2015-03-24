package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.*;

import java.util.*;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 6, 2010<br><br>
 */
public class OWLIndividualByClassHierarchyProvider implements HierarchyProvider<OWLObject> {

    private OWLServer server;

    private Map<OWLClassExpression, Set<OWLIndividual>> cache;

    private ActiveOntologyProvider.Listener serverListener = new ActiveOntologyProvider.Listener(){
        public void activeOntologyChanged(OWLOntology ont) {
            reset();
        }
    };

    private OWLOntologyChangeListener ontologyListener = new OWLOntologyChangeListener(){

        public void ontologiesChanged(List<? extends OWLOntologyChange> changes) throws OWLException {
            for (OWLOntologyChange change : changes){
                if (change.isAxiomChange()){
                    if (change.getAxiom() instanceof OWLClassAssertionAxiom){
                        reset();
                        return;
                    }
                }
            }
        }
    };


    public OWLIndividualByClassHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addActiveOntologyListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
        reset();
    }


    public Class<? extends OWLObject> getNodeClass() {
        return OWLNamedIndividual.class;
    }

    public Set<OWLObject> getRoots() {
        return new HashSet<OWLObject>(cache.keySet());
    }

    public boolean isRoot(OWLObject node) {
        return node instanceof OWLClassExpression;
    }

    public boolean isLeaf(OWLObject node) {
        return node instanceof OWLIndividual;
    }


    public Set<OWLObject> getParents(OWLObject node) {
        if (node instanceof OWLClassExpression){
            return Collections.emptySet();
        }
        Set<OWLObject> types = new HashSet<OWLObject>(((OWLIndividual)node).getTypes(getOntologies()));
        if (types.isEmpty()){
            types = Collections.<OWLObject>singleton(server.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
        }
        return types;
    }


    public Set<OWLObject> getChildren(OWLObject node) {
        if (node instanceof OWLIndividual){
            return Collections.emptySet();
        }
        return new HashSet<OWLObject>(cache.get((OWLClassExpression)node));
    }


    public Set<OWLObject> getEquivalents(OWLObject node) {
        if (node instanceof OWLIndividual){
            Set<OWLObject> sameIndividuals = new HashSet<OWLObject>();
            for (OWLOntology ont : getOntologies()){
                sameIndividuals.addAll(((OWLIndividual)node).getSameIndividuals(ont));
            }
            return sameIndividuals;
        }
        return Collections.emptySet();
    }


    public Set<OWLObject> getDescendants(OWLObject node) {
        return getChildren(node);
    }


    public Set<OWLObject> getAncestors(OWLObject node) {
        return getParents(node);
    }

    public boolean hasAncestor(OWLObject node, OWLObject ancestor) {
        return getParents(node).contains(ancestor);
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

        OWLClass owlThing = server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        cache = new HashMap<OWLClassExpression, Set<OWLIndividual>>();

        Set<OWLIndividual> allIndividuals = new HashSet<OWLIndividual>();

        for (OWLOntology ont : getOntologies()){
            allIndividuals.addAll(ont.getIndividualsInSignature());
        }

        for (OWLOntology ont : getOntologies()){
            for (OWLClassAssertionAxiom ax : ont.getAxioms(AxiomType.CLASS_ASSERTION)){
                if (!ax.getClassExpression().equals(owlThing)){
                    Set<OWLIndividual> inds = cache.get(ax.getClassExpression());
                    if (inds == null){
                        inds = new HashSet<OWLIndividual>();
                        cache.put(ax.getClassExpression(), inds);
                    }

                    inds.add(ax.getIndividual());

                    allIndividuals.remove(ax.getIndividual());
                }
            }
        }

        // any individuals left with no asserted type are added to owl:Thing
        cache.put(owlThing, allIndividuals);
    }
}

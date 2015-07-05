package org.coode.owl.hierarchy;

import com.google.common.collect.Sets;
import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.*;

public class OWLObjectPropertyHierarchyProvider implements HierarchyProvider<OWLObjectProperty> {

    private OWLServer server;

    private Set<OWLObjectProperty> implicitRoots;

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


    public OWLObjectPropertyHierarchyProvider(OWLServer server) {
        this.server = server;
        server.addActiveOntologyListener(serverListener);
        server.getOWLOntologyManager().addOntologyChangeListener(ontologyListener);
    }

    private OWLObjectProperty getTopProperty() {
        return getServer().getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty();
    }

    private Set<OWLObjectProperty> getPropertiesInSignature(OWLOntology ont){
        return ont.getObjectPropertiesInSignature();
    }

    protected OWLServer getServer() {
        return server;
    }

    @Override
    public Class<? extends OWLObjectProperty> getNodeClass() {
        return OWLObjectProperty.class;
    }

    public Set<OWLObjectProperty> getRoots() {
        return Collections.singleton(getTopProperty());
    }

    public boolean isRoot(OWLObjectProperty node) {
        return node.equals(getTopProperty());
    }

    public boolean isLeaf(OWLObjectProperty node) {
        return getChildren(node).isEmpty();
    }

    public boolean hasAncestor(OWLObjectProperty node, OWLObjectProperty ancestor) {
        return getAncestors(node).contains(ancestor);
    }

    public Set<OWLObjectProperty> getParents(OWLObjectProperty node) {
        Set<OWLObjectProperty> supers;
        if (isRoot(node)){
            supers = Collections.emptySet();
        }
        else{
            supers = Sets.newHashSet();
            for (OWLObjectPropertyExpression pe :  EntitySearcher.getSuperProperties(node, getOntologies())){
                if (!pe.isAnonymous()) {
                    supers.add(pe.asOWLObjectProperty());
                }
            }
            if (supers.isEmpty()){
                supers.addAll(getRoots());
            }
        }
        return supers;
    }

    public Set<OWLObjectProperty> getChildren(OWLObjectProperty node) {
        if (isRoot(node)){
            return getImplicitRoots();
        }

        Set<OWLObjectProperty> subs = Sets.newHashSet();

        for (OWLObjectPropertyExpression pe :  EntitySearcher.getSubProperties(node, getOntologies())){
            if (!pe.isAnonymous()) {
                subs.add(pe.asOWLObjectProperty());
            }
        }

        return subs;
    }

    public Set<OWLObjectProperty> getEquivalents(OWLObjectProperty node) {
        Set<OWLObjectProperty> equivs = Sets.newHashSet();
        for (OWLObjectPropertyExpression pe :  EntitySearcher.getEquivalentProperties(node, getOntologies())){
            if (!pe.isAnonymous()) {
                equivs.add(pe.asOWLObjectProperty());
            }
        }
        return equivs;
    }


    public Set<OWLObjectProperty> getDescendants(OWLObjectProperty node) {
        if (isRoot(node)){
            return getAllReferencedProperties();
        }

        Set<OWLObjectProperty> descendants = Sets.newHashSet();
        List<OWLObjectProperty> nodes = new ArrayList<OWLObjectProperty>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (OWLObjectProperty child : getChildren(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        descendants.addAll(nodes);
        descendants.remove(node);
        return descendants;
    }

    public Set<OWLObjectProperty> getAncestors(OWLObjectProperty node) {
        if (isRoot(node)){
            return Collections.emptySet();
        }

        Set<OWLObjectProperty> ancestors;
        List<OWLObjectProperty> nodes = new ArrayList<OWLObjectProperty>();
        nodes.add(node);
        for(int i=0; i<nodes.size(); i++){
            for (OWLObjectProperty child : getParents(nodes.get(i))){
                if (!nodes.contains(child)){
                    nodes.add(child);
                }
            }
        }
        if (nodes.size() == 1){
            ancestors = getRoots();
        }
        else{
            ancestors = new HashSet<OWLObjectProperty>(nodes);
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

    private Set<OWLObjectProperty> getAllReferencedProperties() {
        Set<OWLObjectProperty> props = Sets.newHashSet();
        for (OWLOntology ont : getOntologies()){
            props.addAll(getPropertiesInSignature(ont));
        }
        return props;
    }

    private Set<OWLObjectProperty> getImplicitRoots() {
        if (implicitRoots == null){
            implicitRoots = getAllReferencedProperties();
            implicitRoots.removeAll(getRoots());
            for (Iterator<OWLObjectProperty> i=implicitRoots.iterator(); i.hasNext();){
                OWLObjectProperty p = i.next();
                final Iterable<OWLObjectPropertyExpression> supers = EntitySearcher.getSuperProperties(p, getOntologies());
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
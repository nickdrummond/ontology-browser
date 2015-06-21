package org.coode.owl.hierarchy;

import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class OntologyHierarchyProvider implements HierarchyProvider<OWLOntology> {

    private OWLServer server;

    public OntologyHierarchyProvider(OWLServer server) {
        this.server = server;
    }

    public Class<? extends OWLOntology> getNodeClass() {
        return OWLOntology.class;
    }

    public Set<OWLOntology> getRoots() {
        return Collections.singleton(server.getRootOntology());
    }

    public boolean isRoot(OWLOntology node) {
        return getRoots().contains(node);
    }

    public boolean isLeaf(OWLOntology node) {
        return getChildren(node).isEmpty();
    }

    public Set<OWLOntology> getChildren(OWLOntology node) {
        return node.getDirectImports();
    }

    public Set<OWLOntology> getEquivalents(OWLOntology node) {
        return Collections.emptySet();
    }

    public Set<OWLOntology> getDescendants(OWLOntology node) {
        return node.getImports();
    }

    public Set<OWLOntology> getParents(OWLOntology node) {
        Set<OWLOntology> parents = new HashSet<OWLOntology>();
        for (OWLOntology parent : server.getOntologies()){
            if (parent.getDirectImports().contains(node)){
                parents.add(parent);
            }
        }
        return parents;
    }

    public Set<OWLOntology> getAncestors(OWLOntology node) {
        Set<OWLOntology> ancestors = new HashSet<OWLOntology>();
        for (OWLOntology ancestor : server.getOntologies()){
            if (ancestor.getImports().contains(node)){
                ancestors.add(ancestor);
            }
        }
        return ancestors;
    }

    public boolean hasAncestor(OWLOntology node, OWLOntology ancestor) {
        return !node.equals(ancestor) && ancestor.getImports().contains(node);
    }

    public void dispose() {
        server = null;
    }
}

package org.coode.owl.mngr.impl;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerRuntimeException;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 6, 2009<br><br>
 */
public class ClassHierarchyProvider implements HierarchyProvider<OWLClass>{

    private Logger logger = Logger.getLogger(ClassHierarchyProvider.class);

    private OWLServer server;

    public ClassHierarchyProvider(OWLServer server) {
        this.server = server;
        logger.setLevel(Level.WARN);
    }

    public Class<? extends OWLClass> getNodeClass() {
        return OWLClass.class;
    }

    public Set<OWLClass> getRoots() {
        return Collections.singleton(getOWLThing());
    }

    public boolean isRoot(OWLClass node) {
        return node.equals(getOWLThing());
    }

    public boolean isLeaf(OWLClass node) {
        return getChildren(node).isEmpty();
    }

    public Set<OWLClass> getParents(OWLClass node) {
        try {
            return getReasoner().getSuperClasses(node, true).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }

    public Set<OWLClass> getChildren(OWLClass node) {
        logger.debug("getChildren(" + node + ")");
        try {
            Set<OWLClass> children = new HashSet<OWLClass>();

            NodeSet<OWLClass> subsets = getReasoner().getSubClasses(node, true);
            for (Node<OWLClass> synset : subsets.getNodes()){
                if (!synset.isBottomNode()){
                    children.addAll(synset.getEntities());
                }
            }
            return children;
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }

    public Set<OWLClass> getEquivalents(OWLClass node) {
        logger.debug("getEquivalents(" + node + ")");
        try{
            return getReasoner().getEquivalentClasses(node).getEntitiesMinus(node);
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }

    public Set<OWLClass> getDescendants(OWLClass node) {
        logger.debug("getDescendants(" + node + ")");
        try {
            return getReasoner().getSubClasses(node, false).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }

    public Set<OWLClass> getAncestors(OWLClass node) {
        try{
            return getReasoner().getSuperClasses(node, false).getFlattened();
        }
        catch (OWLReasonerRuntimeException e) {
            logger.error(e);
        }
        return Collections.emptySet();
    }

    public boolean hasAncestor(OWLClass node, OWLClass ancestor) {
        return getAncestors(node).contains(ancestor);
    }

    public void dispose() {
    }

    protected final OWLServer getServer() {
        return server;
    }

    protected Set<OWLOntology> getOntologies() {
        return getServer().getActiveOntologies();
    }

    protected OWLReasoner getReasoner() {
        return getServer().getOWLReasoner();
    }

    private OWLClass getOWLThing() {
        return server.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
    }
}

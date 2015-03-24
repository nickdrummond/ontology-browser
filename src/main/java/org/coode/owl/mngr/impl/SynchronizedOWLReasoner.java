package org.coode.owl.mngr.impl;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.util.Version;

import java.util.List;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 24, 2007<br><br>
 *
 * Wrapper for any OWLReasoner that implements methods as synchronized to allow safe multithread access
 */
public class SynchronizedOWLReasoner implements OWLReasoner {

    private final OWLReasoner r;

    public SynchronizedOWLReasoner(OWLReasoner r) {
        this.r = r;
    }

    public OWLReasoner getDelegate() {
        return r;
    }

    public synchronized void dispose() {
        r.dispose();
    }

    public synchronized void flush() {
        r.flush();
    }

    public synchronized Node<OWLClass> getBottomClassNode() {
        return r.getBottomClassNode();
    }

    public synchronized Node<OWLDataProperty> getBottomDataPropertyNode() {
        return r.getBottomDataPropertyNode();
    }

    public synchronized BufferingMode getBufferingMode() {
        return r.getBufferingMode();
    }

    public synchronized NodeSet<OWLClass> getDataPropertyDomains(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDataPropertyDomains(pe, direct);
    }

    public synchronized Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual ind, OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDataPropertyValues(ind, pe);
    }

    public synchronized NodeSet<OWLNamedIndividual> getDifferentIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDifferentIndividuals(ind);
    }

    public synchronized Node<OWLClass> getEquivalentClasses(OWLClassExpression ce) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentClasses(ce);
    }

    public synchronized Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentDataProperties(pe);
    }

    public synchronized FreshEntityPolicy getFreshEntityPolicy() {
        return r.getFreshEntityPolicy();
    }

    public synchronized IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
        return r.getIndividualNodeSetPolicy();
    }

    public synchronized NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getInstances(ce, direct);
    }

    public synchronized NodeSet<OWLClass> getObjectPropertyDomains(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getObjectPropertyDomains(pe, direct);
    }

    public synchronized NodeSet<OWLClass> getObjectPropertyRanges(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getObjectPropertyRanges(pe, direct);
    }

    public synchronized NodeSet<OWLNamedIndividual> getObjectPropertyValues(OWLNamedIndividual ind, OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getObjectPropertyValues(ind, pe);
    }

    public synchronized Set<OWLAxiom> getPendingAxiomAdditions() {
        return r.getPendingAxiomAdditions();
    }

    public synchronized Set<OWLAxiom> getPendingAxiomRemovals() {
        return r.getPendingAxiomRemovals();
    }

    public synchronized List<OWLOntologyChange> getPendingChanges() {
        return r.getPendingChanges();
    }

    public synchronized String getReasonerName() {
        return r.getReasonerName();
    }

    public synchronized Version getReasonerVersion() {
        return r.getReasonerVersion();
    }

    public synchronized OWLOntology getRootOntology() {
        return r.getRootOntology();
    }

    public synchronized Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSameIndividuals(ind);
    }

    public synchronized NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct) {
        return r.getSubClasses(ce, direct);
    }

    public synchronized NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSubDataProperties(pe, direct);
    }


    public synchronized NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSuperClasses(ce, direct);
    }

    public synchronized NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSuperDataProperties(pe, direct);
    }

    public synchronized long getTimeOut() {
        return r.getTimeOut();
    }

    public synchronized Node<OWLClass> getTopClassNode() {
        return r.getTopClassNode();
    }

    public synchronized Node<OWLDataProperty> getTopDataPropertyNode() {
        return r.getTopDataPropertyNode();
    }

    public synchronized NodeSet<OWLClass> getTypes(OWLNamedIndividual ind, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getTypes(ind, direct);
    }

    public synchronized Node<OWLClass> getUnsatisfiableClasses() throws ReasonerInterruptedException, TimeOutException {
        return r.getUnsatisfiableClasses();
    }

    public synchronized void interrupt() {
        r.interrupt();
    }

    public synchronized boolean isConsistent() throws ReasonerInterruptedException, TimeOutException {
        return r.isConsistent();
    }

    public synchronized boolean isEntailed(OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return r.isEntailed(axiom);
    }

    public synchronized boolean isEntailed(Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return r.isEntailed(axioms);
    }

    public synchronized boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
        return r.isEntailmentCheckingSupported(axiomType);
    }

    public synchronized boolean isSatisfiable(OWLClassExpression classExpression) throws ReasonerInterruptedException, TimeOutException, ClassExpressionNotInProfileException, FreshEntitiesException, InconsistentOntologyException {
        return r.isSatisfiable(classExpression);
    }


// OWLAPI v3.0
//    public synchronized NodeSet<OWLObjectProperty> getDisjointObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getDisjointObjectProperties(pe, direct);
//    }
//
//    public synchronized Node<OWLObjectProperty> getEquivalentObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getEquivalentObjectProperties(pe);
//    }
//
//    public synchronized Node<OWLObjectProperty> getInverseObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getInverseObjectProperties(pe);
//    }
//
//    public synchronized Node<OWLObjectProperty> getTopObjectPropertyNode() {
//        return r.getTopObjectPropertyNode();
//    }
//
//    public synchronized NodeSet<OWLObjectProperty> getSubObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getSubObjectProperties(pe, direct);
//    }
//
//    public synchronized NodeSet<OWLObjectProperty> getSuperObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getSuperObjectProperties(pe, direct);
//    }
//
//    public synchronized NodeSet<OWLClass> getDisjointClasses(OWLClassExpression ce, boolean direct) {
//        return r.getDisjointClasses(ce, direct);
//    }
//
//    public synchronized NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
//        return r.getDisjointDataProperties(pe, direct);
//    }
//
//    public synchronized Node<OWLObjectProperty> getBottomObjectPropertyNode() {
//        return r.getBottomObjectPropertyNode();
//    }
//
//    public void prepareReasoner() throws ReasonerInterruptedException, TimeOutException {
//        r.prepareReasoner();
//    }

    
// OWLAPI v3.1
    public synchronized void precomputeInferences(InferenceType... inferenceTypes) throws ReasonerInterruptedException, TimeOutException, InconsistentOntologyException {
        r.precomputeInferences(inferenceTypes);
    }

    public synchronized boolean isPrecomputed(InferenceType inferenceType) {
        return r.isPrecomputed(inferenceType);
    }

    public synchronized Set<InferenceType> getPrecomputableInferenceTypes() {
        return r.getPrecomputableInferenceTypes();
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDisjointObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getInverseObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getInverseObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
        return r.getTopObjectPropertyNode();
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSubObjectProperties(pe, direct);
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSuperObjectProperties(pe, direct);
    }

    public synchronized NodeSet<OWLClass> getDisjointClasses(OWLClassExpression ce) {
        return r.getDisjointClasses(ce);
    }

    public synchronized NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDisjointDataProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
        return r.getBottomObjectPropertyNode();
    }
}

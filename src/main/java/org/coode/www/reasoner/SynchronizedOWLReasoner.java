package org.coode.www.reasoner;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.util.Version;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.Set;

/**
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

    public synchronized NodeSet<OWLClass> getDataPropertyDomains(@NonNull OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDataPropertyDomains(pe, direct);
    }

    public synchronized Set<OWLLiteral> getDataPropertyValues(@NonNull OWLNamedIndividual ind, @NonNull OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDataPropertyValues(ind, pe);
    }

    public synchronized NodeSet<OWLNamedIndividual> getDifferentIndividuals(@NonNull OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDifferentIndividuals(ind);
    }

    public synchronized Node<OWLClass> getEquivalentClasses(@NonNull OWLClassExpression ce) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentClasses(ce);
    }

    public synchronized Node<OWLDataProperty> getEquivalentDataProperties(@NonNull OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentDataProperties(pe);
    }

    public synchronized FreshEntityPolicy getFreshEntityPolicy() {
        return r.getFreshEntityPolicy();
    }

    public synchronized IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
        return r.getIndividualNodeSetPolicy();
    }

    public synchronized NodeSet<OWLNamedIndividual> getInstances(@NonNull OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getInstances(ce, direct);
    }

    public synchronized NodeSet<OWLClass> getObjectPropertyDomains(@NonNull OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getObjectPropertyDomains(pe, direct);
    }

    public synchronized NodeSet<OWLClass> getObjectPropertyRanges(@NonNull OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getObjectPropertyRanges(pe, direct);
    }

    public synchronized NodeSet<OWLNamedIndividual> getObjectPropertyValues(@NonNull OWLNamedIndividual ind, @NonNull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
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

    public synchronized Node<OWLNamedIndividual> getSameIndividuals(@NonNull OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSameIndividuals(ind);
    }

    public synchronized NodeSet<OWLClass> getSubClasses(@NonNull OWLClassExpression ce, boolean direct) {
        return r.getSubClasses(ce, direct);
    }

    public synchronized NodeSet<OWLDataProperty> getSubDataProperties(@NonNull OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSubDataProperties(pe, direct);
    }


    public synchronized NodeSet<OWLClass> getSuperClasses(@NonNull OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSuperClasses(ce, direct);
    }

    public synchronized NodeSet<OWLDataProperty> getSuperDataProperties(@NonNull OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
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

    public synchronized NodeSet<OWLClass> getTypes(@NonNull OWLNamedIndividual ind, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
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

    public synchronized boolean isEntailed(@NonNull OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return r.isEntailed(axiom);
    }

    public synchronized boolean isEntailed(@NonNull Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return r.isEntailed(axioms);
    }

    public synchronized boolean isEntailmentCheckingSupported(@NonNull AxiomType<?> axiomType) {
        return r.isEntailmentCheckingSupported(axiomType);
    }

    public synchronized boolean isSatisfiable(@NonNull OWLClassExpression classExpression) throws ReasonerInterruptedException, TimeOutException, ClassExpressionNotInProfileException, FreshEntitiesException, InconsistentOntologyException {
        return r.isSatisfiable(classExpression);
    }

    public synchronized void precomputeInferences(@NonNull InferenceType... inferenceTypes) throws ReasonerInterruptedException, TimeOutException, InconsistentOntologyException {
        r.precomputeInferences(inferenceTypes);
    }

    public synchronized boolean isPrecomputed(@NonNull InferenceType inferenceType) {
        return r.isPrecomputed(inferenceType);
    }

    public synchronized Set<InferenceType> getPrecomputableInferenceTypes() {
        return r.getPrecomputableInferenceTypes();
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(@NonNull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDisjointObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(@NonNull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getEquivalentObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getInverseObjectProperties(@NonNull OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getInverseObjectProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
        return r.getTopObjectPropertyNode();
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(@NonNull OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSubObjectProperties(pe, direct);
    }

    public synchronized NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(@NonNull OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getSuperObjectProperties(pe, direct);
    }

    public synchronized NodeSet<OWLClass> getDisjointClasses(@NonNull OWLClassExpression ce) {
        return r.getDisjointClasses(ce);
    }

    public synchronized NodeSet<OWLDataProperty> getDisjointDataProperties(@NonNull OWLDataPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return r.getDisjointDataProperties(pe);
    }

    public synchronized Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
        return r.getBottomObjectPropertyNode();
    }
}

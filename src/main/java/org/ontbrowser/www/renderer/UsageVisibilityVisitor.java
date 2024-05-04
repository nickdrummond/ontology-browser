package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.*;
import org.springframework.lang.NonNull;

import javax.annotation.Nonnull;

public class UsageVisibilityVisitor implements OWLAxiomVisitor {

    private boolean showUsage;
    private OWLEntity currentEntity;

    @Override
    public void visit(@NonNull OWLDeclarationAxiom axiom) {
        showUsage = false;
    }

    public void visit(OWLAnnotationAssertionAxiom axiom) {
        showUsage = !(axiom.getSubject().isIRI() && axiom.getSubject().equals(currentEntity.getIRI()));
    }

    public void visit(OWLSubClassOfAxiom ax) {
        if (ax.getSubClass().isNamed()) {
            if (ax.getSubClass().equals(currentEntity)) {
                showUsage = false; // we'll already be showing it as superclasses
            } else if (ax.getSuperClass().equals(currentEntity)) {
                showUsage = false; // we'll already be showing it as subclasses
            }
        }
    }

    public void visit(OWLDisjointClassesAxiom ax) {
        for (OWLClassExpression d : ax.getClassExpressions()) {
            if (!(d instanceof OWLClass)) {
                return;
            }
        }
        showUsage = false;
    }

    public void visit(OWLEquivalentClassesAxiom ax) {
        for (OWLClassExpression d : ax.getClassExpressions()) {
            if (d.equals(currentEntity)) {
                showUsage = false;
                return;
            }
        }
    }

    public void visit(OWLClassAssertionAxiom ax) {
        // we'll already be showing it as type/member
        showUsage = !(ax.getIndividual().equals(currentEntity) ||
                ax.getClassExpression().equals(currentEntity));
    }

    @Override
    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        showUsage = !axiom.getSubject().equals(currentEntity);
    }

    @Override
    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        showUsage = !axiom.getSubject().equals(currentEntity);
    }

    @Override
    public void visit(@NonNull OWLDifferentIndividualsAxiom ax) {
        visitNaryIndAxiom(ax);
    }

    @Override
    public void visit(@NonNull OWLSameIndividualAxiom ax) {
        visitNaryIndAxiom(ax);
    }

    @Override
    public void visit(@NonNull OWLAsymmetricObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLReflexiveObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLFunctionalObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLSymmetricObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLTransitiveObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLIrreflexiveObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLInverseFunctionalObjectPropertyAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLObjectPropertyDomainAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(@NonNull OWLObjectPropertyRangeAxiom axiom) {
        visitUnaryObjectPropertyAxiom(axiom);
    }

    @Override
    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        showUsage = !((axiom.getFirstProperty().equals(currentEntity) ||
                axiom.getSecondProperty().equals(currentEntity)));
    }

    @Override
    public void visit(OWLSubObjectPropertyOfAxiom axiom) {
        // already showing as Superproperty
        showUsage = !(axiom.getSubProperty().equals(currentEntity));
    }

    @Override
    public void visit (@NonNull OWLFunctionalDataPropertyAxiom axiom){
        visitUnaryDataPropertyAxiom(axiom);
    }

    @Override
    public void visit (@NonNull OWLDataPropertyDomainAxiom axiom){
        visitUnaryDataPropertyAxiom(axiom);
    }

    @Override
    public void visit (@NonNull OWLDataPropertyRangeAxiom axiom){
        visitUnaryDataPropertyAxiom(axiom);
    }

    public boolean getShowUsage (@NonNull OWLAxiom ax, @NonNull OWLEntity entity){
        this.currentEntity = entity;
        showUsage = true;
        ax.accept(this);
        return showUsage;
    }

    public void visitNaryIndAxiom (@NonNull OWLNaryIndividualAxiom ax){
        for (OWLIndividual d : ax.getIndividuals()) {
            if (d.equals(currentEntity)) {
                showUsage = false; // we'll already be showing it as same or different
                return;
            }
        }
    }

    private void visitUnaryObjectPropertyAxiom (@NonNull OWLUnaryPropertyAxiom < OWLObjectPropertyExpression > axiom) {
        showUsage = !axiom.getProperty().equals(currentEntity);
    }

    private void visitUnaryDataPropertyAxiom (@Nonnull OWLUnaryPropertyAxiom < OWLDataPropertyExpression > axiom) {
        showUsage = !axiom.getProperty().equals(currentEntity);
    }
}

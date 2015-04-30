package org.coode.www.renderer;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OWLAxiomVisitorAdapter;

public class UsageVisibilityVisitor extends OWLAxiomVisitorAdapter {

    private boolean showUsage;
    private OWLEntity currentEntity;

    @Override
    public void visit(OWLDeclarationAxiom axiom) {
        showUsage = false;
    }

    public void visit(OWLAnnotationAssertionAxiom axiom) {
        if (axiom.getSubject() instanceof IRI && axiom.getSubject().equals(currentEntity.getIRI())){
            showUsage = false;
        }
    }

    public void visit(OWLSubClassOfAxiom ax) {
        if (ax.getSubClass() instanceof OWLClass){
            if (ax.getSubClass().equals(currentEntity)){
                showUsage = false; // we'll already be showing it as superclasses
            }
            else if (ax.getSuperClass().equals(currentEntity)){
                showUsage = false; // we'll already be showing it as subclasses
            }
        }
    }

    public void visit(OWLDisjointClassesAxiom ax) {
        for (OWLClassExpression d : ax.getClassExpressions()){
            if (!(d instanceof OWLClass)){
                return;
            }
        }
        showUsage = false;
    }


    public void visit(OWLEquivalentClassesAxiom ax) {
        for (OWLClassExpression d : ax.getClassExpressions()){
            if (d.equals(currentEntity)){
                showUsage = false;
                return;
            }
        }
    }

    public void visit(OWLClassAssertionAxiom ax) {
        if (ax.getIndividual().equals(currentEntity) ||
                ax.getClassExpression().equals(currentEntity)){
            showUsage = false; // we'll already be showing it as type/member
        }
    }


    @Override
    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        if (axiom.getSubject().equals(currentEntity)){
            showUsage = false;
        }
    }

    @Override
    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        if (axiom.getSubject().equals(currentEntity)){
            showUsage = false;
        }
    }

    public void visit(OWLDifferentIndividualsAxiom ax) {
        visitNaryIndAxiom(ax);
    }


    public void visit(OWLSameIndividualAxiom ax) {
        visitNaryIndAxiom(ax);
    }

    public void visitNaryIndAxiom(OWLNaryIndividualAxiom ax) {
        for (OWLIndividual d : ax.getIndividuals()){
            if (d.equals(currentEntity)){
                showUsage = false; // we'll already be showing it as same or different
                return;
            }
        }
    }

    public boolean getShowUsage(OWLAxiom ax, OWLEntity entity) {
        this.currentEntity = entity;
        showUsage = true;
        if (ax instanceof OWLUnaryPropertyAxiom){ // too expensive to do each by hand
            if (((OWLUnaryPropertyAxiom)ax).getProperty().equals(currentEntity)){
                showUsage = false;
            }
        }
        else{
            ax.accept(this);
        }
        return showUsage;
    }
}

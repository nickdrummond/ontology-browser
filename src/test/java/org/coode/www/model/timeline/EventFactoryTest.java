package org.coode.www.model.timeline;

import junit.framework.TestCase;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleShortFormProvider;

public class EventFactoryTest extends TestCase {

    @Test
    public void converging() {

    }

    @Test
    public void parallelExample() throws OWLOntologyCreationException {

        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLOntology ont = mngr.createOntology();

        OWLObjectProperty during = df.getOWLObjectProperty("during");
        OWLObjectProperty after = df.getOWLObjectProperty("after");

        AbstractRelationsHierarchyService<OWLObjectProperty> duringTree = new RelationsHierarchyService()
                .withProperties(during, ont, true);

        AbstractRelationsHierarchyService<OWLObjectProperty> afterTree = new RelationsHierarchyService()
                .withProperties(after, ont, true);

        EventFactory eventFactory = new EventFactory(duringTree, afterTree);

        OWLNamedIndividual parent = df.getOWLNamedIndividual("Parent");
        OWLNamedIndividual child1 = df.getOWLNamedIndividual("Child1");
        OWLNamedIndividual child2 = df.getOWLNamedIndividual("Child2");
        OWLNamedIndividual child3 = df.getOWLNamedIndividual("Child3");
        OWLNamedIndividual p2_1 = df.getOWLNamedIndividual("p2_1");
        OWLNamedIndividual p2_2 = df.getOWLNamedIndividual("p2_2");
        OWLNamedIndividual p2_3 = df.getOWLNamedIndividual("p2_3");
        OWLNamedIndividual p3_1 = df.getOWLNamedIndividual("p3_1");
        OWLNamedIndividual p3_2 = df.getOWLNamedIndividual("p3_2");

        // During
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child2, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, child3, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_2, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p2_3, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p3_1, parent));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(during, p3_2, parent));

        // After
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child2, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child3, child2));

        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_1, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_2, p2_1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p2_3, p2_2));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, child3, p2_3));

        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p3_1, child1));
        ont.addAxiom(df.getOWLObjectPropertyAssertionAxiom(after, p3_2, p3_1));

        Timeline timeline = eventFactory.buildTimeline(parent, Integer.MAX_VALUE);


    }

}
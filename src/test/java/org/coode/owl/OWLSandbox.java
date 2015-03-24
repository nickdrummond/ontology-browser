package org.coode.owl;

import junit.framework.TestCase;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;

import java.net.URI;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 15, 2011<br><br>
 */
public class OWLSandbox extends TestCase {

    public void testStructuralSubsOfOWLThing(){
        try {
            OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
            OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl"));
            OWLReasoner r = new StructuralReasonerFactory().createNonBufferingReasoner(ont);
            final NodeSet<OWLClass> subs = r.getSubClasses(mngr.getOWLDataFactory().getOWLThing(), true);
            System.out.println("subs = " + subs);
            assertEquals(3, subs.getNodes().size());
        }
        catch (OWLOntologyCreationException e) {
            e.printStackTrace();
            fail();
        }
    }


    public void testStructuralSubsOfOWLThingUsingManager(){
        try {
            OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
            OWLServer server = new OWLServerImpl(mngr);

            server.loadOntology(URI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl"));

            HierarchyProvider<OWLClass> hp = server.getHierarchyProvider(OWLClass.class);

            Set<OWLClass> subs = hp.getChildren(mngr.getOWLDataFactory().getOWLThing());

            System.out.println("subs = " + subs);

            assertEquals(2, subs.size());
        }
        catch (OWLOntologyCreationException e) {
            e.printStackTrace();
            fail();
        }
    }
}

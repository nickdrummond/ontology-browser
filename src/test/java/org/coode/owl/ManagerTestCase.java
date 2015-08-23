package org.coode.owl;

import junit.framework.TestCase;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class ManagerTestCase extends TestCase {

    private Logger logger = LoggerFactory.getLogger(ManagerTestCase.class);

    public void testFindNamedClassDoesNotExist(){

        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        try {
            OWLOntology ont = mngr.createOntology(IRI.create("http://www.co-ode.org/ontologies/test/test.owl"));

            OWLServer server = new OWLServerImpl(mngr);

            OWLDataFactory df = mngr.getOWLDataFactory();
            OWLClass a = df.getOWLClass(IRI.create("http://www.co-ode.org/ontologies/test/test.owl#A"));
            OWLClass b = df.getOWLClass(IRI.create("http://www.co-ode.org/ontologies/test/test.owl#B"));
            OWLSubClassOfAxiom bSubA = df.getOWLSubClassOfAxiom(b, a);
            List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
            changes.add(new AddAxiom(ont, bSubA));
            mngr.applyChanges(changes);

            Set<OWLClass> matchesForA = server.getFinder().getOWLClasses("A");
            assertTrue(matchesForA.size() == 1);
            assertSame(a, matchesForA.iterator().next());

            Set<OWLClass> matchesForZ = server.getFinder().getOWLClasses("Z");
            assertTrue(matchesForZ.isEmpty());
        }
        catch (Exception e) {
            logger.error("Fail!", e);
            fail();
        }
    }
//
//    public void testOWLLinkConnection(){
//        try {
//            OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
//            OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create("http://www.co-ode.org/ontologies/pizza/pizza.owl"));
//
//            assertNotNull(ont);
//
//            OWLReasonerConfiguration conf = new OWLlinkReasonerConfiguration(new URL("http://localhost:1234"));
//            OWLlinkReasoner r = new OWLlinkHTTPXMLReasonerFactory().createReasoner(ont, conf);
//
//            for (Node node : r.getSubClasses(mngr.getOWLDataFactory().getOWLThing(), true)){
//                System.out.println("node = " + node);
//            }
//        }
//        catch (Exception e) {
//            e.printStackTrace();
//            fail();
//        }
//    }


//    public void testPelletReload(){
//        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
//        try {
//            OWLOntology ont = mngr.createOntology(URI.create("http://www.co-ode.org/ontologies/test/test.owl"));
//            OWLDataFactory df = mngr.getOWLDataFactory();
//            OWLClass a = df.getOWLClass(URI.create("http://www.co-ode.org/ontologies/test/test.owl#A"));
//            OWLClass b = df.getOWLClass(URI.create("http://www.co-ode.org/ontologies/test/test.owl#B"));
//            OWLSubClassOfAxiom bSubA = df.getOWLSubClassOfAxiom(b, a);
//            List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();
//            changes.add(new AddAxiom(ont, bSubA));
//            mngr.applyChanges(changes);
//
//            Reasoner r = new Reasoner(mngr);
//            r.loadOntology(ont);
//            r.classify();
//
//            Set<Set<OWLClass>> results = r.getSubClasses(a);
//            for (Set<OWLClass> result : results){
//                logger.debug("result = " + result);
//            }
//
//            changes.clear();
//            changes.add(new RemoveAxiom(ont, bSubA));
//            mngr.applyChanges(changes);
//
////            r.refresh(); // this solves the problem but not part of OWL API interface - Everin contacted 03/10/2007
//            r.classify();
//
//            results = r.getSubClasses(a); // still contains B
//            for (Set<OWLClass> result : results){
//                logger.debug("result = " + result);
//            }
//        }
//        catch (OWLOntologyCreationException e) {
//            logger.error(e);
//            fail();
//        }
//        catch (OWLOntologyChangeException e) {
//            logger.error(e);
//            fail();
//        }
//    }
}


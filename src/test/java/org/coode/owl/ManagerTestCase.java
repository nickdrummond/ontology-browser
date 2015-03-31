package org.coode.owl;

import junit.framework.TestCase;
import org.slf4j.LoggerFactory; import org.slf4j.Logger;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
//import org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory;
//import org.semanticweb.owlapi.owllink.OWLlinkReasoner;
//import org.semanticweb.owlapi.owllink.OWLlinkReasonerConfiguration;
//import org.semanticweb.owlapi.reasoner.Node;
//import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;

//import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Oct 3, 2007<br><br>
 */
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


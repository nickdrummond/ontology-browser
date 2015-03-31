package org.coode.www;

import junit.framework.TestCase;
import org.slf4j.LoggerFactory; import org.slf4j.Logger;
import org.semanticweb.owlapi.util.NamespaceUtil;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 4, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class OfflineTestCase extends TestCase {

    private static final Logger logger = LoggerFactory.getLogger(OfflineTestCase.class.getName());

//    public void testImports(){
//        OWLOntologyManager mngr = org.semanticweb.owlapi.apibinding.OWLManager.createOWLOntologyManager();
//        try {
//            // B imports A
//            IRI a = IRI.create(getClass().getResource("A.owl").toURI());
//            IRI b = IRI.create(getClass().getResource("B.owl").toURI());
//            mngr.loadOntologyFromOntologyDocument(b);
//
//            assertEquals(2, mngr.getOntologies().size());
//
//            mngr.loadOntologyFromOntologyDocument(a);
//
//            for (OWLOntology ont : mngr.getOntologies()){
//                logger.debug("ont = " + ont);
//                logger.debug("classes = " + ont.getClassesInSignature());
//            }
//
//            assertEquals(2, mngr.getOntologies().size());
//        }
//        catch (Exception e) {
//            logger.error(e);
//            fail();
//        }
//    }

    public void testNamespaceGeneration(){
        String ontURI = "http://cohse.semanticweb.org/ontologies/people";
        String ns = new NamespaceUtil().generatePrefix(ontURI);
        assertEquals("people", ns);
    }

//    public void testRenderHTMLToString(){
//        try {
//            URI b = getClass().getResource("B.owl").toURI();
//
//            OWLHTMLKit kit = new OWLHTMLKitImpl("dsdsdsd", new URL("http://www.co-ode.org/ontologies/")){
//                public OWLReasoner getOWLReasoner() {
//                    return null;  //@@TODO implement
//                }
//            };
//            OWLHTMLRenderer ren = new OWLHTMLRenderer(kit);
//            OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
//            kit.getOWLServer().loadOntology(b);
//
//            String str = ren.render(df.getOWLClass(IRI.create("http://www.co-ode.org/ontologies/b.owl#class1")), null);
//            logger.debug("str = " + str);
//        }
//        catch (Exception e) {
//            logger.error(e);
//            fail();
//        }
//    }
}

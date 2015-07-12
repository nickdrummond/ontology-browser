package org.coode.www.service;

import org.coode.www.model.Tree;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasoner;

import java.util.Comparator;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.coode.www.service.Matchers.*;

/**
 * Integration test using StructuralReasoner implementation.
 * To check that the behaviour of the reasoner and tree builder is correct.
 *
 */
public class OWLClassHierarchyServiceTest {

    private OWLClassHierarchyService service;

    private OWLDataFactory dataFactory;

    private OWLReasoner reasoner;

    private OWLOntology ontology;

    private OWLOntologyManager mngr;

    private OWLClass owlThing, a, a2, b, b2, b3, b4, c;

    private Comparator<? super Tree<OWLClass>> comparator;

    @Before
    public void setup() throws OWLOntologyCreationException {
        mngr = OWLManager.createOWLOntologyManager();
        dataFactory = mngr.getOWLDataFactory();
        owlThing = dataFactory.getOWLThing();

        ontology = mngr.createOntology();
        reasoner = new StructuralReasoner(ontology, new SimpleConfiguration(), BufferingMode.NON_BUFFERING);
        comparator = (o1, o2) -> o1.value.getRepresentativeElement().compareTo(o2.value.getRepresentativeElement());
        service = new OWLClassHierarchyService(reasoner, comparator);

        a = cls("a");
        a2 = cls("a2");
        b = cls("b");
        b2 = cls("b2");
        b3 = cls("b3");
        b4 = cls("b4");
        c = cls("c");
    }

    private void addAxiom(OWLAxiom axiom) {
        mngr.applyChange(new AddAxiom(ontology, axiom));
    }

    private OWLClass cls(String name) {
        return dataFactory.getOWLClass(IRI.create(name));
    }

    private void subs(OWLClass superCls, OWLClass... subClses) {
        for (OWLClass sub: subClses) {
            addAxiom(dataFactory.getOWLSubClassOfAxiom(sub, superCls));
        }
    }

    private void equiv(final OWLClass... clses) {
        addAxiom(dataFactory.getOWLEquivalentClassesAxiom(clses));
    }

    @Test
    public void owlThing() {
        Tree<OWLClass> hierarchy = service.getPrunedTree(owlThing);

        assertThat(hierarchy, looksLike(
                t(owlThing)
        ));
    }

    /**
     * owl:Thing
     * - a <- requested
     */
    @Test
    public void inferredRoots() {
        addAxiom(dataFactory.getOWLDeclarationAxiom(a));

        Tree<OWLClass> hierarchy = service.getPrunedTree(a);

        assertThat(hierarchy.value.isTopNode(), equalTo(true));

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a)
                )
        ));
    }

    /**
     * owl:Thing <- requested
     * - a <- gets subclasses
     */
    @Test
    public void subclasses() {
        addAxiom(dataFactory.getOWLDeclarationAxiom(a));

        Tree<OWLClass> hierarchy = service.getPrunedTree(owlThing);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a)
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - b <- requested
     */
    @Test
    public void simplePath() {

        subs(owlThing, a);
        subs(a, b);

        Tree<OWLClass> hierarchy = service.getPrunedTree(b);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(b)
                        )
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - b <- requested
     *   - b2
     * - a2
     */
    @Test
    public void keepsSiblings() {

        subs(owlThing, a, a2);
        subs(a, b, b2);

        Tree<OWLClass> hierarchy = service.getPrunedTree(b);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(b),
                                t(b2)
                        ),
                        t(a2)
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - b  <- pruned
     *   - b2 <- pruned
     * - a2
     *   - b3
     *   - b4 <- requested
     *     - c <- not pruned
     */
    @Test
    public void prunes() {
        subs(owlThing, a, a2);
        subs(a, b, b2);
        subs(a2, b3, b4);
        subs(b4, c);

        Tree<OWLClass> hierarchy = service.getPrunedTree(b4);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a),
                        t(a2,
                                t(b3),
                                t(b4,
                                        t(c)
                                )
                        )
                )
        ));
    }

    /**
     * owl:Thing
     * - a
     *   - [b, b2]
     *   - [b3, b4] <- requested
     */
    @Test
    public void equivalents() {
        subs(owlThing, a);
        subs(a, b, b3);
        equiv(b, b2);
        equiv(b3, b4);

        Tree<OWLClass> hierarchy = service.getPrunedTree(b4);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a,
                                t(all(b, b2)),
                                t(all(b3, b4))
                        )
                )
        ));
    }
}

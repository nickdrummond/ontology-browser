package org.coode.www.service;

import com.google.common.collect.Sets;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasoner;

import java.util.Comparator;
import java.util.Set;

import static org.coode.www.service.OWLClassHierarchyService.Tree;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.coode.www.service.Matchers.*;

/**
 * Integration test using OWLReasoner implementation.
 */
public class OWLClassHierarchyServiceTest {

    private OWLClassHierarchyService service;

    private OWLDataFactory dataFactory;

    private OWLReasoner reasoner;

    private OWLOntology ontology;

    private OWLOntologyManager mngr;

    private OWLClass owlThing, a, a2, a3, b, b2, b3, b4, c;

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
        a3 = cls("a3");
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

    private void build(OWLClass superCls, Set<OWLClass> subClses) {
        for (OWLClass sub: subClses) {
            addAxiom(dataFactory.getOWLSubClassOfAxiom(sub, superCls));
        }
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
     * owl:Thing
     * - a
     *   - b <- requested
     */
    @Test
    public void simplePath() {

        build(owlThing, Sets.newHashSet(a));
        build(a, Sets.newHashSet(b));

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

        build(owlThing, Sets.newHashSet(a, a2));
        build(a, Sets.newHashSet(b, b2));

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
     *     - c <- pruned
     */
    @Test
    public void prunes() {
        build(owlThing, Sets.newHashSet(a, a2));
        build(a, Sets.newHashSet(b, b2));
        build(a2, Sets.newHashSet(b3, b4));
        build(b4, Sets.newHashSet(c));

        Tree<OWLClass> hierarchy = service.getPrunedTree(b4);

        assertThat(hierarchy, looksLike(
                t(owlThing,
                        t(a),
                        t(a2,
                                t(b3),
                                t(b4)
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
    public void equivalentClasses() {
        build(owlThing, Sets.newHashSet(a));
        build(a, Sets.newHashSet(b, b3));
        addAxiom(dataFactory.getOWLEquivalentClassesAxiom(b, b2));
        addAxiom(dataFactory.getOWLEquivalentClassesAxiom(b3, b4));

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

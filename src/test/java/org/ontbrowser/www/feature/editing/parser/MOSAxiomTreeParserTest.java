package org.ontbrowser.www.feature.editing.parser;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.ontbrowser.www.feature.parser.axiom.MOSAxiomTreeParser;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;

import javax.annotation.Nullable;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotNull;

public class MOSAxiomTreeParserTest {

    static Stream<String> data() {
        return Stream.of(
                "ClassA SubClassOf ClassB",
                "ObjectPropertyA Domain ClassA",
                "ObjectPropertyA Range ClassB",
                "DataPropertyA Domain ClassA",
                "DataPropertyA Range DataTypeA",
                "ClassA EquivalentTo ClassB",
                "ObjectPropertyA InverseOf ObjectPropertyB",
                "ClassA DisjointWith ClassB",
                "ClassA SubClassOf (ObjectPropertyA some ClassB)",
                "ClassA SubClassOf (DataPropertyA value \"someValue\")",
                "ClassA SubClassOf (ObjectPropertyA only ClassB)",
                "ClassA SubClassOf (ObjectPropertyA min 1 ClassB)",
                "ClassA SubClassOf (ObjectPropertyA max 2 ClassB)",
                "ClassA SubClassOf (ObjectPropertyA exactly 3 ClassB)",
                "ClassA SubClassOf (ObjectPropertyA some ClassB) and (DataPropertyA value \"someValue\")",
                "ClassA SubClassOf (ObjectPropertyA some ClassB) and (DataPropertyA value \"someValue\") or ClassC",
                "ClassA SubClassOf (ObjectPropertyA some (ClassB or ClassC))",
                "ClassA SubClassOf (ObjectPropertyA some (ClassB and ClassC))",
                "ClassA SubClassOf (ObjectPropertyA some (ClassB and not ClassC))"
        );
    }

    @ParameterizedTest
    @MethodSource("data")
    public void testAxiomParse(String axiomString) {
        var manager = OWLManager.createOWLOntologyManager();
        var df = manager.getOWLDataFactory();
        var parser = new MOSAxiomTreeParser(df, new TestOWLEntityChecker(df));

        var axiom = parser.parse(axiomString);
        System.out.println("axiom = " + axiom);
        assertNotNull(axiom, "Parsed axiom should not be null");
    }


    private static class TestOWLEntityChecker implements OWLEntityChecker {
        private static final String BASE = "http://null.org/";
        private final OWLDataFactory df;

        public TestOWLEntityChecker(OWLDataFactory df) {
            this.df = df;
        }

        @Nullable
        @Override
        public OWLClass getOWLClass(String name) {
            if (name.startsWith("Class")) {
                return df.getOWLClass(BASE + name);
            }
            return null;
        }

        @Nullable
        @Override
        public OWLObjectProperty getOWLObjectProperty(String name) {
            if (name.startsWith("ObjectProperty")) {
                return df.getOWLObjectProperty(BASE + name);
            }
            return null;
        }

        @Nullable
        @Override
        public OWLDataProperty getOWLDataProperty(String name) {
            if (name.startsWith("DataProperty")) {
                return df.getOWLDataProperty(BASE + name);
            }
            return null;
        }

        @Nullable
        @Override
        public OWLNamedIndividual getOWLIndividual(String name) {
            if (name.startsWith("Individual")) {
                return df.getOWLNamedIndividual(BASE + name);
            }
            return null;
        }

        @Nullable
        @Override
        public OWLDatatype getOWLDatatype(String name) {
            if (name.startsWith("DataType")) {
                return df.getOWLDatatype(BASE + name);
            }
            return null;
        }

        @Nullable
        @Override
        public OWLAnnotationProperty getOWLAnnotationProperty(String name) {
            if (name.startsWith("AnnotationProperty")) {
                return df.getOWLAnnotationProperty(BASE + name);
            }
            return null;
        }
    }
}
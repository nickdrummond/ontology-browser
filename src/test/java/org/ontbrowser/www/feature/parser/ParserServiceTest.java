package org.ontbrowser.www.feature.parser;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.IRI;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;

public class ParserServiceTest {

    @Test
    public void shouldParseGoodOWLExpression() {
        var validOWLExpression = "hasTopping some PizzaTopping";

        var df = new OWLDataFactoryImpl();
        var checker = Mockito.mock(OWLEntityChecker.class);

        var aClass = df.getOWLClass(IRI.create("http://example.com/class"));
        var aProperty = df.getOWLObjectProperty(IRI.create("http://example.com/class"));

        Mockito.when(checker.getOWLClass("PizzaTopping")).thenReturn(aClass);
        Mockito.when(checker.getOWLObjectProperty("hasTopping")).thenReturn(aProperty);

        var service = new ParserService();

        var results = service.parse(validOWLExpression, df, checker);

        assertEquals(ParseStatus.OK, results.status());
    }

    @Test
    public void shouldThrowExceptionIfBadOWLExpression() {
        var validOWLExpression = "hasTopping some Piz";

        var df = new OWLDataFactoryImpl();
        var checker = Mockito.mock(OWLEntityChecker.class);

        var aProperty = df.getOWLObjectProperty(IRI.create("http://example.com/class"));

        Mockito.when(checker.getOWLClass(anyString())).thenReturn(null);
        Mockito.when(checker.getOWLObjectProperty("hasTopping")).thenReturn(aProperty);

        var service = new ParserService();

        var result = service.parse(validOWLExpression, df, checker);
        assertEquals(16, result.startPos());
    }


    // TODO tests for autocomplete
}

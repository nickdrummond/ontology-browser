package org.coode.www.service;

import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

import org.junit.Test;
import org.mockito.Mockito;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.anyString;

public class ParserServiceTest {

    @Test
    public void shouldParseGoodOWLExpression() throws ParseException {
        String validOWLExpression = "hasTopping some PizzaTopping";

        OWLDataFactory df = new OWLDataFactoryImpl();
        OWLEntityChecker checker = Mockito.mock(OWLEntityChecker.class);

        OWLClass aClass = df.getOWLClass(IRI.create("http://example.com/class"));
        OWLObjectProperty aProperty = df.getOWLObjectProperty(IRI.create("http://example.com/class"));

        Mockito.when(checker.getOWLClass("PizzaTopping")).thenReturn(aClass);
        Mockito.when(checker.getOWLObjectProperty("hasTopping")).thenReturn(aProperty);

        ParserService service = new ParserService();

        ParseResult results = service.parse(validOWLExpression, df, checker);

        assertThat(results.toString(), containsString("OK"));
    }

    @Test
    public void shouldThrowExceptionIfBadOWLExpression() throws ParseException {
        String validOWLExpression = "hasTopping some Piz";

        OWLDataFactory df = new OWLDataFactoryImpl();
        OWLEntityChecker checker = Mockito.mock(OWLEntityChecker.class);

        OWLObjectProperty aProperty = df.getOWLObjectProperty(IRI.create("http://example.com/class"));

        Mockito.when(checker.getOWLClass(anyString())).thenReturn(null);
        Mockito.when(checker.getOWLObjectProperty("hasTopping")).thenReturn(aProperty);

        ParserService service = new ParserService();

        try {
            service.parse(validOWLExpression, df, checker);
            fail("Should have thrown ParseException");
        }
        catch (ParseException e) {
            assertThat(e.toString(), not(containsString("OK")));
            assertThat(e.toString(), containsString("error  pos=\"16\""));
        }
    }


    // TODO tests for autocomplete
}

/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.OWLExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxClassExpressionParser;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import java.text.ParseException;

/**
 * This implementation instantiates a Man Syntax Class Expression parser
 * on creation.
 * We do not manage changes to the ontology. It is assumed that the
 * DataFactory and EntityChecker updating is sufficient.
 */
public class ManchesterOWLSyntaxParser implements OWLClassExpressionParser {

    private final OWLExpressionParser<OWLClassExpression> parser;

    public ManchesterOWLSyntaxParser(OWLServer server) {
        this.parser = getParser(server);
    }

    public OWLClassExpression parse(String str) {
        return parser.parse(str);
    }

    private OWLExpressionParser<OWLClassExpression> getParser(OWLServer server){
        final OWLOntologyManager ontMngr = server.getOWLOntologyManager();
        final OWLDataFactory df = ontMngr.getOWLDataFactory();

        final OWLEntityChecker checker = server.getOWLEntityChecker();

        return new ManchesterOWLSyntaxClassExpressionParser(df, checker);
    }
}
/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxEditorParser;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.NamespaceUtil;

import java.text.ParseException;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 11, 2008<br><br>
 */
public class ManchesterOWLSyntaxParser implements OWLClassExpressionParser {

    private OWLServer server;

    // @@TODO this should probably be shared (ie the server should have it) so that renderers can use it too
    private NamespaceUtil nsUtil;

    
    public ManchesterOWLSyntaxParser(OWLServer server) {
        this.server = server;
        nsUtil = new NamespaceUtil();
    }


    public OWLClassExpression parse(String str) throws ParseException {
        try {
            return getParser(str).parseClassExpression();
        }
        catch (ParserException e) {
            throw new ParseException(e.getMessage(), e.getStartPos());
        }
    }


    private ManchesterOWLSyntaxEditorParser getParser(String expression){
        final OWLOntologyManager ontMngr = server.getOWLOntologyManager();

        OWLEntityChecker checker = server.getOWLEntityChecker();

        final OWLDataFactory df = ontMngr.getOWLDataFactory();

        ManchesterOWLSyntaxEditorParser parser = new ManchesterOWLSyntaxEditorParser(df, expression);
        parser.setDefaultOntology(server.getActiveOntology());
        parser.setOWLEntityChecker(checker);

        return parser;
    }
}
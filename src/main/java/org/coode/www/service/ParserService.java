package org.coode.www.service;

import org.coode.owl.mngr.OWLClassExpressionParser;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerConstants;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

public class ParserService {

    public ParseResult parse(final String expression, final OWLHTMLKit kit) throws ParseException {

        OWLClassExpressionParser parser = kit.getOWLServer().getClassExpressionParser(ServerConstants.Syntax.man.toString());

        try {
            parser.parse(expression);
            return new ParseResult(expression, "OK");
        } catch (ParserException e) {
            throw new ParseException(expression, e.getMessage(), e.getStartPos(), e.getCurrentToken());
        }
    }
}

package org.ontbrowser.www.feature.parser;

import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;

enum ParseStatus {
    OK,
    ERROR
}

public record ParseResultJson (ParseStatus status, String expression, String message, int startPos, String currentToken) {
    public static ParseResultJson ok(String expression) {
        return new ParseResultJson(ParseStatus.OK, expression, "OK", 0, "");
    }

    public static ParseResultJson error(String expression, ParserException ex) {
        return new ParseResultJson(ParseStatus.ERROR, expression, ex.getMessage(), ex.getStartPos(), ex.getCurrentToken());
    }
}

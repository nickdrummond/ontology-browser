package org.ontbrowser.www.feature.dlquery;

enum ParseStatus {
    OK,
    ERROR
}

public record ParseResultJson (ParseStatus status, String expression, String message, int startPos, String currentToken) {
}

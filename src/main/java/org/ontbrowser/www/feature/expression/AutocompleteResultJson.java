package org.ontbrowser.www.feature.expression;

import java.util.List;
import java.util.Map;

public record AutocompleteResultJson(String expression, int pos, String lastToken, Map<String, List<String>> expected) {
}

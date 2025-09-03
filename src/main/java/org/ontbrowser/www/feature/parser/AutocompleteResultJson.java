package org.ontbrowser.www.feature.parser;

import java.util.List;
import java.util.Map;

public record AutocompleteResultJson(String expression, int pos, String lastToken, Map<String, List<String>> expected) {
}

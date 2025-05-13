package org.ontbrowser.www.util;

import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MyStringUtils {

    private MyStringUtils() {}

    // see https://www.baeldung.com/java-regex-token-replacement
    public static String replace(String original, Pattern pattern, Function<Matcher, String> converter) {
        int lastIndex = 0;
        StringBuilder output = new StringBuilder();
        Matcher matcher = pattern.matcher(original);
        while (matcher.find()) {
            output.append(original, lastIndex, matcher.start())
                    .append(converter.apply(matcher));
            lastIndex = matcher.end();
        }
        if (lastIndex < original.length()) {
            output.append(original, lastIndex, original.length());
        }
        return output.toString();
    }

    public static String stripQuotes(String s) {
        return (s.startsWith("\"") && (s.endsWith("\""))) ? s.substring(1, s.length() - 1) : s;
    }

    public static String sanitiseForRegex(final String s) {
        String[] symbols = {"[", "*", "+", "?", "{", "}", ".", "(", ")", "^", "$", "|", "]", "-"};
        String result = s;
        for (String symbol : symbols) {
            result = result.replace(symbol, "\\" + symbol);
        }
        return result;
    }

    // https://stackoverflow.com/questions/2559759/how-do-i-convert-camelcase-into-human-readable-names-in-java
    public static String splitCamelCase(String s) {
        return s.replaceAll(
                String.format("%s|%s|%s",
                        "(?<=[A-Z])(?=[A-Z][a-z])",
                        "(?<=[^A-Z])(?=[A-Z])",
                        "(?<=[A-Za-z])(?=[^A-Za-z])"
                ),
                " "
        );
    }
}

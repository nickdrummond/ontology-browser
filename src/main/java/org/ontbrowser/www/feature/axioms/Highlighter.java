package org.ontbrowser.www.feature.axioms;

/**
 * Put highlight markup around all matching portions of text.
 * All strings should be treated as simple - ie no regex
 */
class Highlighter {

    private static final String START = "<span class=\"highlight\">";
    private static final String END = "</span>";

    private final String highlight;

    public Highlighter(String highlight) {
        this.highlight = highlight;
    }

    public String highlight(String source) {
        if (containsMarkup(source))
            return highlightMarkup(source);
        else
            return highlightSimple(source);
    }

    // pos is how far through a match we currently are
    private String highlightMarkup(String remains) {

        if (remains.isEmpty()) {
            return "";
        }

        // shortcut when we can't find a match
        if (remains.length() < highlight.length()) {
            return remains;
        }

        int pos = 0;
        StringBuilder result = new StringBuilder();

        while (!remains.isEmpty()) {
            int nextTagStart = remains.indexOf("<");

            // if we start with a tag, or inside a tag, skip it
            if (nextTagStart == 0) {
                int endOfTag = remains.indexOf(">") + 1;
                String tagText = remains.substring(0, endOfTag);
                result.append(tagText);
                remains = remains.substring(endOfTag);
            } else if (pos == 0) { // not started matching yet
                // otherwise, see if the highlight text exists in the stripped out text
                String stripped = stripMarkup(remains);
                int startOfHighlight = stripped.toLowerCase().indexOf(highlight.toLowerCase());

                if (startOfHighlight == -1) {
                    result.append(remains);
                    remains = "";
                } else if (nextTagStart == -1) { // no more tags and the highlight must exist
                    String matchedText = remains.substring(startOfHighlight, startOfHighlight + highlight.length()); // in original case
                    result.append(remains, 0, startOfHighlight);
                    result.append(START);
                    result.append(matchedText);
                    result.append(END);
                    result.append(remains.substring(startOfHighlight + highlight.length()));
                    remains = "";
                } else {
                    // is the beginning of the highlight in this text before the next tag?
                    int posInHighlight = nextTagStart - startOfHighlight;
                    if (posInHighlight > 0) {
                        if (posInHighlight >= highlight.length()) {
                            String matchedText = remains.substring(startOfHighlight, startOfHighlight + highlight.length()); // in original case
                            result.append(remains, 0, startOfHighlight);
                            result.append(START);
                            result.append(matchedText);
                            result.append(END);
                            remains = remains.substring(startOfHighlight + highlight.length());
                        } else {
                            String matchedText = remains.substring(startOfHighlight, startOfHighlight + posInHighlight); // in original case
                            result.append(remains, 0, startOfHighlight);
                            result.append(START);
                            result.append(matchedText);
                            result.append(END);
                            remains = remains.substring(nextTagStart);
                            pos = posInHighlight;
                        }
                    } else {
                        // bad because the next call has to find the match all over again (can we have a negative pos?)
                        result.append(remains, 0, nextTagStart);
                        remains = remains.substring(nextTagStart);
                    }
                }
            } else {
                int remainingLen = highlight.length() - pos;
                if (nextTagStart == -1 || nextTagStart >= remainingLen) {
                    result.append(START);
                    result.append(remains, 0, remainingLen);
                    result.append(END);
                    remains = remains.substring(remainingLen);
                    pos = 0;
                } else { // another tag
                    result.append(START);
                    result.append(remains, 0, nextTagStart);
                    result.append(END);
                    remains = remains.substring(nextTagStart);
                    pos = pos + nextTagStart;
                }
            }
        }

        return result.toString();
    }

    private String stripMarkup(String source) {
        return source.replaceAll("<[^>]*>", "");
    }

    private boolean containsMarkup(String source) {
        return source.contains("<");
    }

    private String highlightSimple(String source) {
        int start = source.toLowerCase().indexOf(highlight.toLowerCase());
        if (start == -1) {
            return source;
        }
        var sb = new StringBuilder();
        sb.append(source, 0, start);
        sb.append(START);
        sb.append(highlight);
        sb.append(END);
        sb.append(highlightSimple(source.substring(start + highlight.length())));
        return sb.toString();
    }
}

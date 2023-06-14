package org.coode.www.renderer;

/**
 * Put highlight markup around all matching portions of text.
 * All strings should be treated as simple - ie no regex
 */
public class Highlighter {

    private static final String START = "<span class=\"highlight\">";
    private static final String END = "</span>";

    private String highlight;

    public Highlighter(String highlight) {
        this.highlight = highlight;
    }

    public void setHighlight(String highlight) {
        this.highlight = highlight;
    }

    public String highlight(String source) {
        if (containsMarkup(source))
            return highlightMarkup(source, 0);
        else
            return highlightSimple(source);
    }

    // pos is how far through a match we currently are
    private String highlightMarkup(String source, int pos) {

        if (source.isEmpty()) {
            return "";
        }

        // shortcut when we can't find a match
        if (pos == 0 && source.length() < highlight.length()) {
            return source;
        }

        int nextTagStart = source.indexOf("<");

        // if we start with a tag, skip it
        if (nextTagStart == 0) {
            int endOfTag = source.indexOf(">") + 1; //assuming we didn't start in the middle of a tag - TODO TEST
            String tagText = source.substring(0, endOfTag);
            return tagText + highlightMarkup(source.substring(endOfTag), pos);
        }

        if (pos == 0) { // not started matching yet
            // otherwise, see if the highlight text exists in the stripped out text
            String stripped = stripMarkup(source);
            int startOfHighlight = stripped.indexOf(highlight);
            if (startOfHighlight == -1) {
                return source;
            }

            if (nextTagStart == -1) { // no more tags and the highlight must exist
                return source.substring(0, startOfHighlight) + START + highlight + END + source.substring(startOfHighlight + highlight.length());
            }

            // is the beginning of the highlight in this text before the next tag?
            int posInHighlight = nextTagStart - startOfHighlight;
            if (posInHighlight > 0) {
                if (posInHighlight >= highlight.length()) {
                    return source.substring(0, startOfHighlight) +
                            START +
                            highlight +
                            END +
                            highlightMarkup(source.substring(startOfHighlight+highlight.length()), 0);
                }
                else {
                    return source.substring(0, startOfHighlight) +
                            START +
                            highlight.substring(0, posInHighlight) +
                            END +
                            highlightMarkup(source.substring(nextTagStart), posInHighlight);
                }
            }
            else {
                // bad because the next call has to find the match all over again (can we have a negative pos?)
                return source.substring(0, nextTagStart) + highlightMarkup(source.substring(nextTagStart), 0);
            }
        }
        else {
            int remainingLen = highlight.length() - pos;
            if (nextTagStart == -1 || nextTagStart >= remainingLen) {
                return START +
                        highlight.substring(pos) +
                        END +
                        highlightMarkup(source.substring(remainingLen), 0);
            }
            else { // another tag
                return START +
                        highlight.substring(pos, pos+nextTagStart) +
                        END +
                        highlightMarkup(source.substring(nextTagStart), pos+nextTagStart);
            }
        }
    }

    private String stripMarkup(String source) {
        return source.replaceAll("\\<[^>]*>","");
    }

    private boolean containsMarkup(String source) {
        return source.contains("<");
    }

    private String highlightSimple(String source) {
        int start = source.indexOf(highlight);
        if (start == -1) {
            return source;
        }
        return source.substring(0, start) + START + highlight + END + highlightSimple(source.substring(start + highlight.length()));

    }
}

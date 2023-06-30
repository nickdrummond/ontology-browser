package org.coode.www.renderer;

public class HighlightingHTMLRenderer<O> implements ElementRenderer<O> {
    private final Highlighter highlighter;
    private final ElementRenderer<O> delegate;

    public HighlightingHTMLRenderer(Highlighter highlighter, ElementRenderer<O> delegate) {
        this.highlighter = highlighter;
        this.delegate = delegate;
    }

    @Override
    public String render(O object) {
        return highlighter.highlight(delegate.render(object));
    }
}

package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.renderer.ElementRenderer;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLObject;

public class HighlightingHTMLRenderer<O> implements ElementRenderer<O> {
    private final Highlighter highlighter;
    private final ElementRenderer<O> delegate;

    public static ElementRenderer<OWLObject> getHighlightRenderer(String s, OWLHTMLRenderer delegate){
        return new HighlightingHTMLRenderer<>(new Highlighter(s), delegate);
    }

    public HighlightingHTMLRenderer(Highlighter highlighter, ElementRenderer<O> delegate) {
        this.highlighter = highlighter;
        this.delegate = delegate;
    }

    @Override
    public String render(O object) {
        return highlighter.highlight(delegate.render(object));
    }

    @Override
    public String renderAsPlainString(O object) {
        return delegate.renderAsPlainString(object);
    }
}

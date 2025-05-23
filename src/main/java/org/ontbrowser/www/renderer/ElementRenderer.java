package org.ontbrowser.www.renderer;

public interface ElementRenderer<O> {

    String render(O object);

    String renderAsPlainString(O object);
}

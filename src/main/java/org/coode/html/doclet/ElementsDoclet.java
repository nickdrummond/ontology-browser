/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.renderer.ElementRenderer;

import java.io.PrintWriter;
import java.net.URL;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public abstract class ElementsDoclet<O, E> extends AbstractHTMLDoclet<O> {

    public enum Format {list, csv}

    private String name;
    private final Format format;

    private Comparator<? super E> comparator;


    public ElementsDoclet(String name, Format format){
        this.name = name;
        this.format = format;
    }

    public String getID(){
        return name;
    }

    protected abstract Collection<E> getElements();


    protected abstract ElementRenderer<? super E> getElementRenderer();


    public final void setComparator(Comparator<? super E> comparator){
        this.comparator = comparator;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        Collection<E> objects = getElements();
        if (!objects.isEmpty()){

            renderBoxStart(getID(), out, pageURL);

            ElementRenderer<? super E> elementRenderer = getElementRenderer();

            if (comparator != null){
                objects = new ArrayList<E>(objects);
                Collections.sort((List<E>)objects, comparator);
            }

            switch(format){
                case list:
                    out.println("<ul>");
                    for (E object : objects){
                        out.print("<li");
                        String cls = getCSSClass(object);
                        if (cls != null){
                            out.print(" class=\"");
                            out.print(cls);
                            out.print("\"");
                        }
                        out.print(">");
                        elementRenderer.render(object, pageURL, out);
                        out.println("</li>");
                    }
                    out.println("</ul>");
                    break;
                case csv:
                    for (Iterator<E> i = objects.iterator(); i.hasNext();) {
                        E object = i.next();
                        String cls = getCSSClass(object);
                        if (cls != null){
                            out.print("<span class=\"");
                            out.print(cls);
                            out.print("\">");
                        }
                        elementRenderer.render(object, pageURL, out);
                        if (cls != null){
                            out.print("</span>");
                        }
                        if (i.hasNext()){
                            out.println(",");
                        }
                    }
                    break;
            }
        }
    }

    protected String getCSSClass(E object) {
        return null;
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        if (!getElements().isEmpty()){        
            renderBoxEnd(getID(), out);
        }
    }
}

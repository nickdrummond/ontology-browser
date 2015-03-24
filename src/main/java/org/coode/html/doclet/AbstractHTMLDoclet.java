/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.page.HTMLPage;
import org.coode.html.util.HTMLUtils;

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
 *
 * Default handling for doclet nesting, including handing the user object down through the hierarchy.
 * Content will be surrounded by whatever is created in renderHeader and renderFooter
 */
public abstract class AbstractHTMLDoclet<O> implements NestedHTMLDoclet<O> {

    private boolean pinned = false;

    private List<HTMLDoclet<O>> doclets = new ArrayList<HTMLDoclet<O>>();

    private O userObject;

    protected abstract void renderHeader(URL pageURL, PrintWriter out);
    protected abstract void renderFooter(URL pageURL, PrintWriter out);

    public void renderContent(URL pageURL, PrintWriter out) {
        for (HTMLDoclet doclet : doclets){
            doclet.renderAll(pageURL, out);
        }
    }

    public final void renderAll(URL pageURL, PrintWriter out) {
        try{
            renderHeader(pageURL, out);
            renderContent(pageURL, out);
            renderFooter(pageURL, out);
        }
        catch (Throwable e) {
            e.printStackTrace();
            out.println("<p>" + e.getMessage() + "</p>");
        }
    }

    public final void addDoclet(HTMLDoclet<O> doclet){
        if (!doclet.isPinned()){
            doclet.setUserObject(userObject);
        }
        doclets.add(doclet);
    }

    public final void addDoclet(HTMLDoclet<O> doclet, int index){
        if (!doclet.isPinned()){
            doclet.setUserObject(userObject);
        }
        doclets.add(index, doclet);
    }

    public final void removeDoclet(HTMLDoclet doclet) {
        doclets.remove(doclet);
    }

    public final HTMLDoclet getDoclet(String id) {
        for (HTMLDoclet doclet : doclets){
            if (doclet.getID().equals(id)){
                return doclet;
            }
        }
        return null;
    }

    public int getSubDocletCount() {
        return doclets.size();
    }

    protected List<HTMLDoclet<O>> getDoclets(){
        return Collections.unmodifiableList(doclets);
    }

    public void clear() {
        doclets.clear();
    }

    public final int indexOf(HTMLDoclet doclet) {
        return doclets.indexOf(doclet);
    }

    public void setUserObject(O object){
        this.userObject = object;
        for (HTMLDoclet<O> doclet : doclets){
            if (!doclet.isPinned()){
                doclet.setUserObject(object);
            }
        }
    }

    public final O getUserObject(){
        return this.userObject;
    }

    public void setPinned(boolean pinned){
        this.pinned = pinned;
    }

    public boolean isPinned(){
        return pinned;
    }

    public boolean isFullPage() {
        return false;
    }

    public HTMLPage asPage() {
        return null; // not a page
    }

    public Set<URL> getRequiredCSS() {
        Set<URL> css = new HashSet<URL>();
        for (HTMLDoclet doclet : doclets){
            css.addAll(doclet.getRequiredCSS());
        }
        return css;
    }

    public List<URL> getRequiredJS() {
        List<URL> js = new ArrayList<URL>();
        for (HTMLDoclet doclet : doclets){
            js.addAll(doclet.getRequiredJS());
        }
        return js;
    }

    protected final void renderBoxStart(String name, PrintWriter out, URL pageURL) {
        String id = name != null ? name.toLowerCase().replace(" ", "_") : "ID" + new Random().nextLong();
        renderBoxStart(name, id, out, pageURL);
    }

    protected void renderBoxStart(String name, String id, PrintWriter out, URL pageURL) {
        HTMLUtils.renderBoxStart(name, id, out, pageURL);        
    }

    protected final void renderBoxEnd(String name, PrintWriter out) {
        HTMLUtils.renderBoxEnd(name, out);
    }
}

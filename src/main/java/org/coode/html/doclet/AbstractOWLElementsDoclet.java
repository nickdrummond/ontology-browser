/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.www.renderer.ElementRenderer;
import org.coode.www.renderer.MediaRenderer;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
@Deprecated
public abstract class AbstractOWLElementsDoclet<O extends OWLObject, E extends OWLObject> extends ElementsDoclet<O, E> {

    private static final String INFERRED_CSS_CLASS = "inferred";
    private static final String ASSERTED_CSS_CLASS = "asserted";

    private OWLHTMLKit kit;

    private Set<OWLOntology> ontologies;

    private ElementRenderer<? super E> renderer;

    public AbstractOWLElementsDoclet(String name, Format format, OWLHTMLKit kit) {
        super(name, format);
        this.kit = kit;
        this.renderer = new MediaRenderer(kit);
        setComparator(kit.getOWLServer().getComparator());
    }

    protected final OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }

    public void setOntologies(Set<OWLOntology> onts){
        this.ontologies = onts;
    }

    protected final Collection<E> getElements(){
        Set<OWLOntology> onts = ontologies;
        if (onts == null){
            onts = getOWLHTMLKit().getVisibleOntologies();
        }
        Set<E> elements = new HashSet<E>();
        elements.addAll(getAssertedElements(onts));
        if (showInferences()){
            elements.addAll(getInferredElements(onts));
        }
        return elements;
    }

    private boolean showInferences() {
        return getOWLHTMLKit().getHTMLProperties().isSet(OWLHTMLProperty.optionShowInferences);
    }

    protected abstract Collection<E> getAssertedElements(Set<OWLOntology> ontologies);

    // TODO: make abstract?
    protected Collection<E> getInferredElements(Set<OWLOntology> ontologies){
        return Collections.emptySet();
    }

    @Override
    protected String getCSSClass(E object) { // TODO: should we cache to prevent multiple queries?
        if (showInferences() && !getAssertedElements(getOWLHTMLKit().getVisibleOntologies()).contains(object)){
            return INFERRED_CSS_CLASS;
        }
        else{
            return ASSERTED_CSS_CLASS;
        }
    }

    protected final ElementRenderer<? super E> getElementRenderer() {
        return renderer;
    }

    public String getID() {
        return super.getID() + " (" + getElements().size() + ")";
    }

}

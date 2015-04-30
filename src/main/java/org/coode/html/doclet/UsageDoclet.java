/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OWLAxiomVisitorAdapter;

import java.util.Collection;
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
public class UsageDoclet<O extends OWLEntity> extends AbstractOWLElementsDoclet<O, OWLObject>{

    private UsageVisibilityVisitor usageVisibilityVisitor = new UsageVisibilityVisitor();

    public UsageDoclet(OWLHTMLKit kit) {
        super("Usage", ElementsDoclet.Format.list, kit);
    }

    protected Collection<OWLObject> getAssertedElements(Set<OWLOntology> onts) {
        OWLEntity entity = getUserObject();
        Collection<OWLObject> usage = new HashSet<OWLObject>();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : ont.getReferencingAxioms(entity)){
                if (usageVisibilityVisitor.getShowUsage(ax, entity)){
                    usage.add(ax);
                }
            }

        }

        if (entity instanceof OWLAnnotationProperty){
            for (OWLOntology ont : onts){
                for (OWLAnnotation annot : ont.getAnnotations()){
                    if (annot.getProperty().equals(entity)){
                        usage.add(annot);
                    }
                }
            }
        }
        return usage;
    }
}

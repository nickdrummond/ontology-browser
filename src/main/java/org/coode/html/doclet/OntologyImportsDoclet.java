/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 */
public class OntologyImportsDoclet extends AbstractOWLElementsDoclet<OWLOntology, IRI> {

    public OntologyImportsDoclet(OWLHTMLKit kit) {
        super("Imports", Format.list, kit);
    }

    protected Collection<IRI> getAssertedElements(Set<OWLOntology> onts) {
        Set<IRI> iris = new HashSet<IRI>();
        for (OWLImportsDeclaration decl : getUserObject().getImportsDeclarations()){
            iris.add(decl.getIRI());
        }
        return iris;
    }
}

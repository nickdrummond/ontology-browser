package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.*;

import java.util.Set;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Oct 2, 2007<br><br>
 *
 * Interface describing string -> named object behaviour.
 * Up to the implementation whether this is an absolute match based on eg:
 * - string version of URI
 * - string rendering of human readable name
 * - partial string match (for search)
 */
public interface OWLEntityFinder {

    Set<OWLClass> getOWLClasses(String str);

    Set<OWLObjectProperty> getOWLObjectProperties(String str);

    Set<OWLDataProperty> getOWLDataProperties(String str);

    Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str);

    Set<OWLNamedIndividual> getOWLIndividuals(String str);

    Set<OWLDatatype> getOWLDatatypes(String str);

    Set<OWLProperty> getOWLProperties(String str);

    Set<OWLEntity> getOWLEntities(String str);

    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type, OWLOntology ont);

    /**
     *
     * @param iri
     *@param type  @return can return a set if the type is entities (an individual and a class could be returned)
     */
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type, OWLOntology ont);

    void dispose();
}

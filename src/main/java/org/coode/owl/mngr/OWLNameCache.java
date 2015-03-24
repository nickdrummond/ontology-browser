package org.coode.owl.mngr;

import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 28, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Extension of OWLNamedObjectFinder that allows querying for all known renderings
 */
public interface OWLNameCache {

    Set<String> getClassNames();

    Set<String> getObjectPropertyNames();

    Set<String> getDataPropertyNames();

    Set<String> getAnnotationPropertyNames();

    Set<String> getIndividualNames();

    Set<String> getDatatypeNames();

    Set<String> getEntityNames();

    Set<String> getNames(NamedObjectType type);


    /**
     * Get the named objects matching the rendering provided
     * @param name
     * @param results an accumulator for the results - implementations should always add into the existing set
     * @param type
     */
    <T extends NamedObjectType> void get(String name, Set<T> results, NamedObjectType type);

    void dispose();    
}

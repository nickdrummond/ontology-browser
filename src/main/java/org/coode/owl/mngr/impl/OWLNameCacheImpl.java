package org.coode.owl.mngr.impl;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 4, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Caching rendering to URI map. Gets rebuilt when the ontology has changed (only when next used).
 * Implementation of finder interface searches on string rendering of URI first
 * and then absolute rendered name (not partial matches)
 *
 * Could be more lazy and only rebuild the indices that are being queried
 */
public class OWLNameCacheImpl{// implements OWLNameCache {
}

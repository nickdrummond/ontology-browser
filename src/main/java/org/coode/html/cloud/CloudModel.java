package org.coode.html.cloud;

import java.net.URL;
import java.util.Comparator;
import java.util.Set;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 15, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public interface CloudModel<O> {

//    void dataChanged();

    int getValue(O entity);

    int getMin();

    int getMax();

    int getRange();

    Set<O> getEntities();

    Set<O> getEntities(int threshold);

//    O getEntity(String rendering);

    String getRendering(O entity);

    Comparator<O> getComparator();

//    void dispose();
//
//    void addChangeListener(ChangeListener modelChangeListener);
//
//    void removeChangeListener(ChangeListener modelChangeListener);

    URL getURL(O entity);

    void reload();

    String getTitle();
}

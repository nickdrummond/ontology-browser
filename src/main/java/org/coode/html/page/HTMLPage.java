package org.coode.html.page;

import org.coode.html.doclet.NestedHTMLDoclet;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 20, 2010<br><br>
 */
public interface HTMLPage<O> extends NestedHTMLDoclet<O> {

    void addOnLoad(String jsAction);
}

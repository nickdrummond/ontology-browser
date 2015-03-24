/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.renderer;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public interface ElementRenderer<O> {

    void render(O object, URL pageURL, PrintWriter out);
}

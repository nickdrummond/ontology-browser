package org.coode.html.doclet;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 20, 2010<br><br>
 */
public interface Doclet {

    void renderAll(URL pageURL, PrintWriter out);
    
}

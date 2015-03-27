package org.coode.html;

import junit.framework.TestCase;
import org.apache.log4j.Logger;
import org.coode.html.impl.OWLHTMLKitImpl;

import java.io.File;
import java.net.URI;
import java.net.URL;
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
 * Date: Oct 3, 2007<br><br>
 */
public class ExportTest extends TestCase {

    private Logger logger = Logger.getLogger(ExportTest.class);

    public void testPerformExport(){
        try {
            URL base = new URL("http://purl.uniprot.org/core/");
            OWLHTMLKit kit = new OWLHTMLKitImpl(base);
            URI source = URI.create("file:////Users/drummond/Work/CO-ODE/Ontologies/uniprot/core.owl");
            kit.getOWLServer().loadOntology(source);
            OntologyExporter exporter = new OntologyExporter(kit);
            File target = new File(URI.create("file:////Users/drummond/temp/testowldoc"));
            File index = exporter.export(target);
            logger.info("index created at: " + index);
        }
        catch (Exception e) {
            logger.error(e);
        }
    }
}

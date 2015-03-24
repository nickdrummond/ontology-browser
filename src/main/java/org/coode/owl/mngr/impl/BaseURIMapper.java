package org.coode.owl.mngr.impl;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
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
 * Date: Sep 17, 2007<br><br>
 *
 * URI Mapper for OWL file loading that takes the top level folder of a URI
 * and uses this as a base to find imported ontologies - so that a set of ontologies
 * can be published together in a single location on the web
 *
 */
public class BaseURIMapper implements OWLOntologyIRIMapper {

    private URI baseURI;


    public BaseURIMapper(URI baseURI) {
        this.baseURI = baseURI;
    }


    public IRI getDocumentIRI(IRI ontologyIRI) {
        String base = getBase(ontologyIRI).toString();
        String ontologyName = ontologyIRI.toString().substring(base.length());
        URI loc = URI.create(baseURI + ontologyName);

        try {
            // see if the location on the web exists
            if (loc.getScheme().equals("http")){
                HttpURLConnection con = (HttpURLConnection)loc.toURL().openConnection();
                con.setRequestMethod("HEAD");
                int response = con.getResponseCode();
                con.disconnect();
                if (response == HttpURLConnection.HTTP_OK){
                    return IRI.create(loc);
                }
            }
            else if (loc.getScheme().equals("file")){
                File file = new File(loc);
                if (file.exists()){
                    return IRI.create(loc);
                }
            }
        }
        catch (IOException e) {
            e.printStackTrace(); // not a URL
        }
        return null;
    }

    private URI getBase(IRI iri){
        String baseURIStr = "";
        String uriParts[] = iri.toString().split("/");
        for (int i=0; i<uriParts.length-1; i++){
            baseURIStr += uriParts[i] + "/";
        }
        return URI.create(baseURIStr);
    }
}

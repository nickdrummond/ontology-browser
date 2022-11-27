package org.coode.owl.mngr.impl;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;

/**
 * URI Mapper for OWL file loading that takes the top level folder of a URI
 * and uses this as a base to find imported ontologies - so that a set of ontologies
 * can be published together in a single location on the web.
 */
public class BaseURIMapper implements OWLOntologyIRIMapper {

    private final URI baseURI;

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

package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.IRI;

public class ServerConstants {

    public static final boolean COOKIE_SESSION_RECOVERY = false;

    public static final String FOAF_NAME = "http://xmlns.com/foaf/0.1/name";

    public static final IRI LATITUDE = IRI.create("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
    public static final IRI LONGITUDE = IRI.create("http://www.w3.org/2003/01/geo/wgs84_pos#long");
    public static final IRI POINT = IRI.create("http://www.georss.org/georss/point");

    public static final IRI ROOT_ONTOLOGY = IRI.create("http://www.manchester.ac.uk/root.owl");

    public static final String ROOT_ONTOLOGY_RENDERING = "All ontologies";


    public static enum Syntax {man, simple, qd}

    // supported renderers
    public static final String RENDERER_FRAG = "frag";
    public static final String RENDERER_LABEL = "label";

}

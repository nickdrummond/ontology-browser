package org.coode.html.url;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleShortFormProvider;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 11, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * URL scheme for static HTML pages.
 *
 * URLs are of the form:
 * serverbase/<type>/<fragment>.html
 *
 *
 */
public class StaticFilesURLScheme extends AbstractURLScheme {

    private static final Logger logger = Logger.getLogger(StaticFilesURLScheme.class.getName());

    private ShortFormProvider shortFormProvider;
    private OntologyIRIShortFormProvider ontologyShortFormProvider;

    private static final String ID_SPLITTER = "___";
    private static final String INDEX_PREFIX = "index-";

    private Map<URL, OWLObject> url2ObjMap = new HashMap<URL, OWLObject>();
    private Map<OWLObject, URL> obj2UrlMap = new HashMap<OWLObject, URL>();



    public StaticFilesURLScheme(OWLHTMLKit kit) {
        super(kit);
        this.shortFormProvider = new SimpleShortFormProvider(); // always use the fragment as it will be safe in URLs
        this.ontologyShortFormProvider = new OntologyIRIShortFormProvider();
    }

    public URL getURLForOWLObject(OWLObject owlObject) {

        URL url = obj2UrlMap.get(owlObject);

        try {
            if (url == null){
                if (owlObject instanceof OWLEntity){
                    OWLEntity owlEntity = (OWLEntity)owlObject;
                    String name = shortFormProvider.getShortForm(owlEntity) + ID_SPLITTER + owlEntity.getIRI().hashCode();
                    name = URLEncoder.encode(name, OWLHTMLConstants.DEFAULT_ENCODING);
                    url = new URL(getBaseURL(), NamedObjectType.getType(owlEntity) + OWLHTMLConstants.SLASH + name + OWLHTMLConstants.DEFAULT_EXTENSION);

                }
                else if (owlObject instanceof OWLOntology){
                    IRI iri = ((OWLOntology)owlObject).getOntologyID().getOntologyIRI();
                    String name = ontologyShortFormProvider.getShortForm(iri) + ID_SPLITTER + iri.hashCode();
                    name = URLEncoder.encode(name, OWLHTMLConstants.DEFAULT_ENCODING);
                    url = new URL(getBaseURL(), NamedObjectType.ontologies + OWLHTMLConstants.SLASH + name + OWLHTMLConstants.DEFAULT_EXTENSION);
                }

                if (url != null){
                    obj2UrlMap.put(owlObject, url);
                    url2ObjMap.put(url, owlObject);
                }
            }
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        catch (UnsupportedEncodingException e) {
            logger.error(e);
        }

        return url;
    }

    public OWLObject getOWLObjectForURL(URL url) {
        return url2ObjMap.get(url);
    }

    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        try {
            return new URL(getBaseURL(), type + OWLHTMLConstants.SLASH + INDEX_PREFIX + ontologyShortFormProvider.getShortForm(ont) + OWLHTMLConstants.DEFAULT_EXTENSION);
        }
        catch (MalformedURLException e) {
            logger.error(e);
        }
        return null;
    }


    /**
     * Overloaded to provide the index.html file (which is necessary if the pages are not being hosted on a server)
     * @param type
     * @return
     */
    public URL getURLForIndex(NamedObjectType type) {
        try {
            return new URL(super.getURLForIndex(type) + OWLHTMLConstants.INDEX_HTML);
        }
        catch (MalformedURLException e) {
            logger.error("Cannot create URL for index: " + type, e);
        }
        return null;
    }

    public void setAdditionalLinkArguments(String s) {
        // do nothing
    }

    public void clearAdditionalLinkArguments() {
        // do nothing
    }
}

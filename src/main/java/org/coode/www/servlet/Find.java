package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.index.OWLObjectIndexDoclet;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.doclet.XMLResultsDoclet;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 20, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Service for getting the named entities matching a certain pattern.
 * This can be hooked up to the autocompleter
 *
 * type - the type of the named thing you are looking up (class, objectproperty etc) - specified in ServerConstants
 * input - the string pattern to match against (regexp, but with * as wildcard)
 * format - xml or html results
 */
public class Find extends AbstractOntologyServerServlet {

    private static final String WILDCARD = "*";


    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<OWLEntity> results = getResults(params, kit.getOWLServer());
        return new XMLResultsDoclet(results, kit);
    }


    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<OWLEntity> results = getResults(params, kit.getOWLServer());
        if (results.size() == 1){
            // just go directly to that page
            OWLNamedObject result = results.iterator().next();
            throw new RedirectException(URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForOWLObject(result)));
        }
        else{
            // show a list of matches
            OWLObjectIndexDoclet ren = new OWLObjectIndexDoclet(kit);
            ren.setTitle("Find Results");
            ren.addAll(results);

            OWLDocPage page = new OWLDocPage(kit);
            page.addDoclet(ren);
            return page;
        }
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<OWLEntity> results = getResults(params, kit.getOWLServer());
        OWLObjectIndexDoclet ren = new OWLObjectIndexDoclet(kit);
        ren.setTitle("Find Results");
        ren.addAll(results);
        return ren;
    }

    private Set<OWLEntity> getResults(Map<OWLHTMLParam, String> params, OWLServer server) throws OntServerException {

        String input = params.get(OWLHTMLParam.input);
        String uri = params.get(OWLHTMLParam.uri);
        String paramOntology = params.get(OWLHTMLParam.ontology);
        String typeP = params.get(OWLHTMLParam.type);

        NamedObjectType type = NamedObjectType.entities;
        if (typeP != null){
            type = NamedObjectType.valueOf(typeP);
        }
        
        Set<OWLEntity> results = new HashSet<OWLEntity>();

        OWLEntityFinder finder = server.getFinder();
        OWLOntology ont = getOntology(paramOntology, server);

        if (uri != null && uri.length() > 0){
            try{
                final IRI iri = IRI.create(new URI(uri));
                if (iri.isAbsolute()){
                    results = new HashSet<OWLEntity>(finder.getOWLEntities(iri, type, ont));
                }
            }
            catch(URISyntaxException e){
                logger.error("Could not find anything called: " + uri);
            }
        }
        else{
            if (input == null){
                input = WILDCARD;
            }
            else if (!input.endsWith(WILDCARD)){
                input = input + WILDCARD;
            }

            results.addAll(finder.getOWLEntities("^" + input.replace(WILDCARD, ".*"), type, ont));

            if (results.isEmpty()){
                results.addAll(finder.getOWLEntities(".*" + input.replace(WILDCARD, ".*"), type, ont));
            }
        }
        return results;
    }

    private OWLOntology getOntology(String ontStr, OWLServer server) throws OntServerException {
        if (ontStr != null){
            try {
                // @@TODO handle anonymous ontologies?
                return server.getOWLOntologyManager().getOntology(IRI.create(new URI(ontStr)));
            }
            catch (URISyntaxException e) {
                logger.error("Cannot find ontology for: " + ontStr, e);
            }
        }
        return null;
    }
}

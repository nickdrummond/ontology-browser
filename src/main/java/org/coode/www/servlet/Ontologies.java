package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.util.OWLUtils;
import org.coode.www.OntologyAction;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.coode.www.mngr.SessionManager;
import org.semanticweb.owlapi.io.UnparsableOntologyException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 3, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 *
 * Cannot subclass AbstractOntologyServerServlet as that checks if an ontology has been loaded
 */
public class Ontologies extends AbstractOntologyServerServlet {

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; //@@TODO implement
    }

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        final String actionValue = params.get(OWLHTMLParam.action);

        if (actionValue == null){
            URL redirect = kit.getURLScheme().getURLForOWLObject(kit.getOWLServer().getActiveOntology());
            throw new RedirectException(URLUtils.createRelativeURL(pageURL, redirect));
        }

        try{
            OntologyAction action = OntologyAction.valueOf(actionValue);
            return handleAction(action, params, kit, pageURL);
        }
        catch(IllegalArgumentException e){
            throw new OntServerException("Unknown action: " + actionValue);
        }
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; //@@TODO implement
    }

    private HTMLPage handleAction(OntologyAction action, Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        OWLServer server = kit.getOWLServer();
        try {
            switch(action){
                case load:
                    boolean clear = Boolean.parseBoolean(params.get(OWLHTMLParam.clear));
                    String redirect = params.get(OWLHTMLParam.redirect);
                    return handleLoad(getURIFromParam(params.get(OWLHTMLParam.uri)), clear, redirect, kit, pageURL);
                case remove:
                    return handleRemove(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
                case reload:
                    return handleReload(getOntologyFromParam(params.get(OWLHTMLParam.uri), server), kit, pageURL);
            }
        }
        catch (URISyntaxException e) {
            throw new OntServerException(e);
        }
        throw new RuntimeException("Missing action handler!!");
    }

    private HTMLPage handleLoad(URI uri, boolean clear, String redirect, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        Set<URI> success = new HashSet<URI>();
        Map<URI, Throwable> fail = new HashMap<URI, Throwable>();

        OWLServer server = kit.getOWLServer();

        OWLOntology ont = null;

        if (clear){
            server.clearOntologies();
        }

        try{
            if (uri.isAbsolute()){
                ont = server.loadOntology(uri);
                success.add(uri);
            }
            else{
                throw new IllegalArgumentException("Ontology URIs must be absolute: " + uri);
            }
        }
        catch(Exception e){
            e.printStackTrace();
            fail.put(uri, e);
        }
        catch (OutOfMemoryError e){
            fail.put(uri, e);
            // clear all ontologies as we are in an unpredictable state
            server.clearOntologies();
            throw new OntServerException("Out of memory trying to load ontologies");
        }


        for (URI f : fail.keySet()){
            String message;
            if (fail.get(f) instanceof UnparsableOntologyException){
                message = "Maybe it is not an ontology/linked data file.";
            }
            else{
                message = fail.get(f).getMessage();
            }
            kit.addUserError("<p>Failed to load: " + uri + "</p><p>" + message + "</p>");
        }

        if (!success.isEmpty()){
            SessionManager.createLabel(kit);
        }

        if (server.getOntologies().size() == 1){
            // TODO fix the redirect - can it be relative?
            throw new RedirectException(URLUtils.createRelativeURL(pageURL, kit.getBaseURL()));
        }

        if (redirect == null){

            // if there is an individual with an IRI matching the ontology that has been loaded
            OWLNamedIndividual ind = OWLUtils.getIndividual(IRI.create(uri), server.getActiveOntologies());
            if (ind != null){
                redirect = URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForOWLObject(ind));
            }

            // else just show the active ontology
            if (redirect == null && ont != null){
                redirect = URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForOWLObject(ont));
            }
        }

        // TODO fix the redirect - can it be relative?
        throw new RedirectException(redirect);
    }


    private HTMLPage handleRemove(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        if (ontology == null){
            sb.append("Could not remove ontology: null");
            sb.append("<p>Ontology not loaded</p>");
        }
        else{
            server.removeOntology(ontology);

            SessionManager.createLabel(kit);
//            sb.append("<p>Removed ");
//            sb.append(server.getOntologyShortFormProvider().getShortForm(ontology));
//            sb.append("</p><p>saved session: [");
//            sb.append(kit.getCurrentLabel());
//            sb.append("]</p>");
        }

        throw new RedirectException(URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForOWLObject(server.getActiveOntology())));
        //return new OntologiesPage(kit, pageURL);
    }


    private HTMLPage handleReload(OWLOntology ontology, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        StringBuilder sb = new StringBuilder();

        OWLServer server = kit.getOWLServer();

        final String ontName = server.getOntologyShortFormProvider().getShortForm(ontology);

        try {
            server.reloadOntology(ontology);

            sb.append("<p>Reloaded ").append(ontName).append("</p>");

        }
        catch (Exception e) {
            sb.append("Could not reload ontology: ").append(ontName);
            kit.addUserError(sb.toString(), e);
        }

        throw new RedirectException(URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForOWLObject(server.getActiveOntology())));

//        return new OntologiesPage(kit, pageURL);
    }

    private URI getURIFromParam(String param) throws URISyntaxException {
        String ontURIStr = param.trim();
        return new URI(ontURIStr);
    }

    private OWLOntology getOntologyFromParam(String param, OWLServer server) throws URISyntaxException {
        IRI iri = IRI.create(getURIFromParam(param));
        return server.getOntologyForIRI(iri);
    }
}

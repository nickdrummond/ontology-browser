package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.SummaryPageFactory;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.index.OWLObjectIndexDoclet;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.html.url.URLScheme;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.util.OWLUtils;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.XMLResultsDoclet;
import org.coode.www.exception.OntServerException;
import org.coode.www.exception.RedirectException;
import org.semanticweb.owlapi.model.*;

import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.*;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 6, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * can take input of the form:
 *
 * <type>/?name=<name>&baseURI=<baseURI>
 * entity/?type=<type>&name=<name>&baseURI=<baseURI>
 * ontologies/?uri=<uri>
 */
public class Summary extends AbstractOntologyServerServlet {

    @Override
    protected Doclet getResults(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL, OntologyBrowserConstants.RequestFormat format) throws OntServerException {
        if (format == OntologyBrowserConstants.RequestFormat.html && getSection(pageURL, kit) != null){
            format = OntologyBrowserConstants.RequestFormat.htmlfrag;
        }
        return super.getResults(params, kit, pageURL, format);
    }

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        String ontology = params.get(OWLHTMLParam.ontology);
        NamedObjectType type = kit.getURLScheme().getType(pageURL);

        Set<OWLObject> results = Collections.emptySet();
        if (uri == null && entityName == null){
            results = getIndexResults(getOntology(ontology, kit), kit, type);
        }

        return new XMLResultsDoclet(results, kit);
        // not yet implemented for entities
    }

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        String ontology = params.get(OWLHTMLParam.ontology);

        final URLScheme urlScheme = kit.getURLScheme();

        NamedObjectType type = urlScheme.getType(pageURL);

        // if a name or uri is specified then redirect to search
        if (uri != null || entityName != null){
            performSearch(type, uri, entityName, ontology, kit, OntologyBrowserConstants.RequestFormat.html, pageURL);
        }
        else{
            OWLObject object = urlScheme.getOWLObjectForURL(pageURL);

            // @@TODO handle summary pages when ontology specified

            if (object == null && ontology == null && isShowMiniHierarchiesEnabled(kit)){
                redirectIfNecessary(kit, pageURL);
            }

            if (object == null){
                OWLObjectIndexDoclet index = getIndexRenderer(type, kit, getOntology(ontology, kit));
                OWLDocPage page = new OWLDocPage(kit);
                page.addDoclet(index);
                return page;
            }
            else {
                return new SummaryPageFactory(kit).getSummaryPage(object);
            }
        }
        throw new RuntimeException("Cannot get here");
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        String uri = params.get(OWLHTMLParam.uri);
        String entityName = params.get(OWLHTMLParam.name);
        String ontology = params.get(OWLHTMLParam.ontology);

        final URLScheme urlScheme = kit.getURLScheme();

        NamedObjectType type = urlScheme.getType(pageURL);

        // if a name or uri is specified then redirect to search
        if (uri != null || entityName != null){
            performSearch(type, uri, entityName, ontology, kit, OntologyBrowserConstants.RequestFormat.htmlfrag, pageURL);
        }
        else{
            OWLObject object = urlScheme.getOWLObjectForURL(pageURL);

            if (object == null){
                return getIndexRenderer(type, kit, getOntology(ontology, kit));
            }

            String section = getSection(pageURL, kit);

            if (section != null){
                HTMLDoclet doclet = kit.getDocletFactory().getDoclet(section);

                if (doclet == null){
                    doclet = kit.getDocletFactory().getDoclet(type.name() + "." + section);
                }
                if (doclet != null){
                    doclet.setUserObject(object);
                    return doclet;
                }
                // TODO: throw 404?
                throw new OntServerException("Cannot find section: " + section);
            }

            return new SummaryPageFactory(kit).getSummaryDoclet(object);
        }
        throw new RuntimeException("Cannot get here");
    }

    private String getSection(URL pageURL, OWLHTMLKit kit) {
        String relativeURL = URLUtils.createRelativeURL(kit.getBaseURL(), pageURL);
        int qStart = relativeURL.indexOf("?");
        if (qStart >= 0){
            relativeURL = relativeURL.substring(0, qStart); // remove the query
        }
        String[] path = relativeURL.split(OWLHTMLConstants.SLASH);
        if (path.length >= 3){
            return path[2]; // always the 3rd element
        }
        return null;
    }

    private void redirectIfNecessary(OWLHTMLKit kit, URL pageURL) throws RedirectException {
        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
        final URLScheme urlScheme = kit.getURLScheme();
        switch(urlScheme.getType(pageURL)){
            case classes:
                throw new RedirectException(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForOWLObject(df.getOWLThing())));
            case objectproperties:
                throw new RedirectException(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForOWLObject(df.getOWLTopObjectProperty())));
            case dataproperties:
                throw new RedirectException(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForOWLObject(df.getOWLTopDataProperty())));
            case annotationproperties:
                final OWLAnnotationProperty prop = getFirstAnnotationProperty(kit);
                if (prop != null){
                    throw new RedirectException(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForOWLObject(prop)));
                }
                break;
            case datatypes:
                throw new RedirectException(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForOWLObject(df.getTopDatatype())));
        }
    }

    private OWLAnnotationProperty getFirstAnnotationProperty(OWLHTMLKit kit) {
        Set<OWLAnnotationProperty> annotationProperties = kit.getOWLServer().getHierarchyProvider(OWLAnnotationProperty.class).getRoots();
        if (!annotationProperties.isEmpty()){
            List<OWLAnnotationProperty> aps = new ArrayList<OWLAnnotationProperty>(annotationProperties);
            Collections.sort(aps, kit.getOWLObjectComparator());
            return aps.get(0);
        }
        return null;
    }

    private void performSearch(NamedObjectType type, String uri, String entityName, String ontology, OWLHTMLKit kit, OntologyBrowserConstants.RequestFormat format, URL pageURL) throws OntServerException {
        try{
            Map<OWLHTMLParam, String> map = new HashMap<OWLHTMLParam, String>();
            map.put(OWLHTMLParam.format, format.name());
            map.put(OWLHTMLParam.type, type.toString());
            if (uri != null){
                map.put(OWLHTMLParam.uri, URLEncoder.encode(uri, OWLHTMLConstants.DEFAULT_ENCODING));
            }
            else{
                map.put(OWLHTMLParam.input, entityName);
            }

            if (ontology != null){
                map.put(OWLHTMLParam.ontology, ontology);
            }

            StringBuilder sb = new StringBuilder("find/");
            sb.append(URLUtils.renderParams(map));

            throw new RedirectException(URLUtils.createRelativeURL(pageURL, kit.getURLScheme().getURLForRelativePage(sb.toString())));
        }
        catch (UnsupportedEncodingException e) {
            throw new OntServerException(e);
        }
    }

    private OWLObjectIndexDoclet getIndexRenderer(NamedObjectType type, OWLHTMLKit kit, OWLOntology ont) throws OntServerException {
        StringBuilder sb = new StringBuilder(type.getPluralRendering());
        if (ont != null){
            sb.append(" referenced in ");
            sb.append(kit.getOWLServer().getOntologyShortFormProvider().getShortForm(ont));
        }
        Set<OWLObject> results = getIndexResults(ont, kit, type);
        OWLObjectIndexDoclet ren = new OWLObjectIndexDoclet(kit);
        ren.setTitle(sb.toString());
        ren.addAll(results);
        return ren;
    }

    private Set<OWLObject> getIndexResults(OWLOntology ont, OWLHTMLKit kit, NamedObjectType type) throws OntServerException {

        Set<OWLObject> results = new HashSet<OWLObject>();

        if (ont != null){ // if ontology specified, just display that one
            results.addAll(OWLUtils.getOWLEntitiesFromOntology(type, ont));
        }
        else if (type.equals(NamedObjectType.ontologies)){
            results.addAll(kit.getOWLServer().getActiveOntologies());
        }
        else{
            // no ontology specified, so display all
            for (OWLOntology ontology : kit.getVisibleOntologies()){
                results.addAll(OWLUtils.getOWLEntitiesFromOntology(type, ontology));
            }
        }
        if (type.equals(NamedObjectType.classes)){
            final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();
            results.add(df.getOWLThing());
        }

        return results;
    }

    private OWLOntology getOntology(String ontStr, OWLHTMLKit kit) throws OntServerException {
        if (ontStr != null){
            return kit.getOWLServer().getOntologyForIRI(IRI.create(ontStr));
        }
        return null;
    }

    private boolean isShowMiniHierarchiesEnabled(OWLHTMLKit kit) {
        return kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowMiniHierarchies);
    }
}

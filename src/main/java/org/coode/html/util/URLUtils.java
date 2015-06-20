package org.coode.html.util;

import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 18, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class URLUtils {

    private static final Logger logger = LoggerFactory.getLogger(URLUtils.class.getName());

    public static final String IMAGES_BASE = "static/images/";

    public static final String EXTERNAL_IMAGE = IMAGES_BASE + "external.png";
    public static final String LOAD_IMAGE = IMAGES_BASE + "download.png";

    public static String createRelativeURL(URL current, URL target) {

        try{
            if (current.equals(target)){
                return "";
            }
            else if (!current.getHost().equals(target.getHost())){ // if host different, can only use absolute URL
                return target.toString();
            }
        }
        catch(Throwable e){
            logger.error("Cannot create relative string from " + current + " to " + target);
        }

        List<String> currentPath = new ArrayList<String>(Arrays.asList(current.getPath().split("/")));
        List<String> targetPath = new ArrayList<String>(Arrays.asList(target.getPath().split("/")));

        // strip off empty path elements at the start of each path
        if (!currentPath.isEmpty() && currentPath.get(0).length() == 0){
            currentPath.remove(0);
        }
        if (!targetPath.isEmpty() && targetPath.get(0).length() == 0){
            targetPath.remove(0);
        }

        // strip all of the common path from each
        while(true){
            if (!currentPath.isEmpty() && !targetPath.isEmpty() &&
                currentPath.get(0).equals(targetPath.get(0))){
                currentPath.remove(0);
                targetPath.remove(0);
            }
            else{
                break;
            }
        }

        StringBuffer relativeURL = new StringBuffer();

        int currentSubCount = currentPath.size();
        if (!current.getPath().endsWith("/")){ // then there must be a file at the end
            currentSubCount--;
        }

        for (int i=0; i<currentSubCount; i++){
            relativeURL.append("../");
        }

        for (String s: targetPath){
            relativeURL.append(s);
            relativeURL.append("/");
        }

//        if (relativeURL.equals("")){
//            relativeURL.append(".");
//        }

        // unless the original path ends in "/", remove it
        int len = relativeURL.length();
        if (len > 0 &&
            relativeURL.charAt(len -1)=='/' &&
            !target.getPath().endsWith("/")){
            relativeURL.deleteCharAt(len -1);
        }

        if (target.getQuery() != null){
            if (len == 0){
                relativeURL.append("./"); // otherwise it won't work
            }
            relativeURL.append("?");
            relativeURL.append(target.getQuery());
        }

        if (relativeURL.length() == 0){ // will be achieved if current = a.com/?something=la   and target = a.com/
            return ".";
        }
        else{
            return relativeURL.toString();
        }
    }

    @Deprecated
    public static void renderURLLinks(URL url, URLScheme urlScheme, Set<OWLOntology> ontologies, PrintWriter out) {
        try{

            out.println(" ");
            
            renderImageLink(urlScheme.getURLForRelativePage(EXTERNAL_IMAGE),
                            "Attempt to open link in another window",
                            url, "urlOption", out);


            // if the ontology at this location has not already been loaded
            if (!containsOntology(ontologies, IRI.create(url.toURI()))){

                final URL loadBaseURL = urlScheme.getURLForIndex(NamedObjectType.ontologies);

                final String img = urlScheme.getURLForRelativePage(LOAD_IMAGE).toString();

                out.println(" ");
                out.println("<form method='POST' style='display:inline' action='"+ loadBaseURL +"'>");
                out.println("<input name='uri' type='hidden' value='" + url + "' />");
                out.println("<input name='redirect' type='hidden' value='" + loadBaseURL + "' />");
                out.println("<input type='image' alt='Attempt to load owl/rdf' src='" + img + "' />");
                out.println("</form>");

//                renderImageLink(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.LOAD_IMAGE),
//                                "Attempt to load owl/rdf",
//                                loadURL, null, "urlOption", true, pageURL, out);
            }
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static boolean containsOntology(Set<OWLOntology> ontologies, IRI iri) {
        for (OWLOntology ont : ontologies) {
            if (iri.equals(ont.getOntologyID().getOntologyIRI().orNull())) {
                return true;
            }
        }
        return false;
    }

    public static void renderImageLink(URL imageURL, String altText, URL href, String cssClass, PrintWriter out) {
            out.print("<a href='" + href + "'");

            if (cssClass != null){
                out.print(" class='" + cssClass + "'");
            }

            out.print(" ><img src=\"");
            out.print(imageURL);
            out.print("\" title=\"");
            out.print(altText);
            out.print("\" /></a>");
    }
}

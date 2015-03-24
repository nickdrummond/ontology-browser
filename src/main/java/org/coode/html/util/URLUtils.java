package org.coode.html.util;

import org.apache.log4j.Logger;
import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerConstants;
import org.semanticweb.owlapi.model.*;

import java.io.PrintWriter;
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
 * Date: Jun 18, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class URLUtils {

    private static final Logger logger = Logger.getLogger(URLUtils.class.getName());

//    public static String createRelativeURL(URL current, URL target) {
//        try {
//            URI currentURI = current.toURI();
//            URI targetURI = target.toURI();
//            URI rel = currentURI.relativize(targetURI);
//            logger.debug("rel = " + rel);
//            return rel.toString();
//        }
//        catch (URISyntaxException e) {
//            logger.error(e);
//        }
//        return "";
//    }

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
            logger.debug(current);
            logger.debug(target);
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
        if (!current.getPath().endsWith(OWLHTMLConstants.SLASH)){ // then there must be a file at the end
            currentSubCount--;
        }

        for (int i=0; i<currentSubCount; i++){
            relativeURL.append("..");
            relativeURL.append(OWLHTMLConstants.SLASH);
        }

        for (String s: targetPath){
            relativeURL.append(s);
            relativeURL.append(OWLHTMLConstants.SLASH);
        }

//        if (relativeURL.equals("")){
//            relativeURL.append(".");
//        }

        // unless the original path ends in "/", remove it
        int len = relativeURL.length();
        if (len > 0 &&
            relativeURL.charAt(len -1)=='/' &&
            !target.getPath().endsWith(OWLHTMLConstants.SLASH)){
            relativeURL.deleteCharAt(len -1);
        }

        if (target.getQuery() != null){
            if (len == 0){
                relativeURL.append("."); // otherwise it won't work
                relativeURL.append(OWLHTMLConstants.SLASH);
            }
            relativeURL.append(OWLHTMLConstants.START_QUERY);
            relativeURL.append(target.getQuery());
        }

        if (relativeURL.length() == 0){ // will be achieved if current = a.com/?something=la   and target = a.com/
            return ".";
        }
        else{
            return relativeURL.toString();
        }
    }

    public static Map<OWLHTMLParam, String> getParams(URL url) {
        Map<OWLHTMLParam, String> paramMap = new HashMap<OWLHTMLParam, String>();
        String query = url.getQuery();
        if (query != null){
            String[] params = query.split(OWLHTMLConstants.PARAM_SEP);
            for (String param : params) {
                String[] pair = param.split(OWLHTMLConstants.EQUALS);
                paramMap.put(OWLHTMLParam.valueOf(pair[0]), pair[1]);
            }
        }
        return paramMap;

    }


    public static String renderParams(Map<OWLHTMLParam, String> map) {
        StringBuilder sb = new StringBuilder();
        for (OWLHTMLParam param : map.keySet()){
            if (sb.length() == 0){
                sb.append(OWLHTMLConstants.START_QUERY);
            }
            else{
                sb.append(OWLHTMLConstants.PARAM_SEP);
            }
            sb.append(param.toString());
            sb.append(OWLHTMLConstants.EQUALS);
            sb.append(map.get(param));
        }
        return sb.toString();
    }

    public static boolean isImageURL(IRI iri) {
        String iriStr = iri.toString();
        return iriStr.endsWith(".png") ||
               iriStr.endsWith(".gif") ||
               iriStr.endsWith(".jpg") ||
               iriStr.endsWith(".jpeg");
    }

    public static boolean isSoundURL(IRI iri) {
        String iriStr = iri.toString();
        return iriStr.endsWith(".mp3") ||
               iriStr.endsWith(".wav");
    }

    public static void renderURLLinks(URL url, OWLHTMLKit kit, URL pageURL, PrintWriter out) {
        try{
            final URL loadBaseURL = kit.getURLScheme().getURLForIndex(NamedObjectType.ontologies);

            // need the redirect to be relative to the load URL
            final String relRedirect = URLUtils.createRelativeURL(loadBaseURL, pageURL);

            StringBuilder sb = new StringBuilder();
            sb.append(loadBaseURL);
            sb.append("?").append(OWLHTMLParam.action).append("=load");
            sb.append("&").append(OWLHTMLParam.uri).append("=").append(URLEncoder.encode(url.toString(), OWLHTMLConstants.DEFAULT_ENCODING));
            sb.append("&").append(OWLHTMLParam.redirect).append("=").append(URLEncoder.encode(relRedirect, OWLHTMLConstants.DEFAULT_ENCODING));

            final URL loadURL = new URL(sb.toString());
            
            out.println(" ");
            
            renderImageLink(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.EXTERNAL_IMAGE),
                            "Attempt to open link in another window",
                            url, OWLHTMLConstants.LinkTarget._blank, "urlOption", true, pageURL, out);

            // if the ontology at this location has not already been loaded
            if (kit.getOWLServer().getOntologyForIRI(IRI.create(url.toURI())) == null){
                out.println(" ");
                renderImageLink(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.LOAD_IMAGE),
                                "Attempt to load owl/rdf",
                                loadURL, null, "urlOption", true, pageURL, out);
            }
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static void renderImageLink(URL imageURL, String altText, URL href, OWLHTMLConstants.LinkTarget target, String cssClass, boolean singleFrame, URL pageURL, PrintWriter out) {
        final String relURL = URLUtils.createRelativeURL(pageURL, href);
            out.print("<a href='" + relURL + "'");

            if (cssClass != null){
                out.print(" class='" + cssClass + "'");
            }

            // if the linktarget is another window or we are in a frames view add the target
            if (target != null && (target == OWLHTMLConstants.LinkTarget._blank || !singleFrame)){
                out.print(" target='" + target + "'");
            }

            out.print(" ><img src=\"");
            out.print(URLUtils.createRelativeURL(pageURL, imageURL));
            out.print("\" title=\"");
            out.print(altText);
            out.print("\" /></a>");
    }

    public static Loc getLocation(OWLEntity owlEntity, Set<OWLOntology> onts) {
        if (onts == null || onts.isEmpty()){
            throw new IllegalArgumentException("Ontologies cannot be empty");
        }

        if (owlEntity.isOWLNamedIndividual()){
            OWLDataFactory df = onts.iterator().next().getOWLOntologyManager().getOWLDataFactory();
            OWLDataProperty latProp = df.getOWLDataProperty(ServerConstants.LATITUDE);
            OWLDataProperty longProp = df.getOWLDataProperty(ServerConstants.LONGITUDE);
            OWLDataProperty point = df.getOWLDataProperty(ServerConstants.POINT);

            Loc loc = new Loc();
            for (OWLOntology ont : onts){
                for (OWLLiteral val : owlEntity.asOWLNamedIndividual().getDataPropertyValues(point, ont)){
                    String[] latLong = val.getLiteral().trim().split("\\s+");
                    if (latLong.length == 2){
                        loc.latitude = latLong[0];
                        loc.longitude = latLong[1];
                        return loc;
                    }
                }
                for (OWLLiteral val : owlEntity.asOWLNamedIndividual().getDataPropertyValues(latProp, ont)){
                    loc.latitude = val.getLiteral().trim();
                    break; // use the first value
                }
                for (OWLLiteral val : owlEntity.asOWLNamedIndividual().getDataPropertyValues(longProp, ont)){
                    loc.longitude = val.getLiteral().trim();
                    break; // use the first value
                }
                if (loc.latitude != null && loc.longitude != null){
                    return loc;
                }
            }
        }
        return null;
    }

    public static class Loc {
        public String latitude;
        public String longitude;
    }
}

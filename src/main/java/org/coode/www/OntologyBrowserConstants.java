/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www;

import org.coode.html.impl.OWLHTMLConstants;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 14, 2008<br><br>
 */
public class OntologyBrowserConstants extends OWLHTMLConstants {

    public static String VERSION = "1.4.4";

    public static final String SERVER_STATES_DIR = "caches/";
    public static final String SERVER_STATES_EXT = ".saved";

    // bookmarks
    public static final String BOOKMARKS_XML = "custom.bookmarks.xml";
    public static final String DEFAULT_BOOKMARKS_XML = "default.bookmarks.xml";

    public static final String LOAD_ONTOLOGIES_INPUT_ID = "uri-spec";

    public static final String DL_QUERY_LABEL = "DL Query";

    public static final String LABEL_COOKIE_NAME = "label";

    public static final URL HELP_PAGE = createURL("http://code.google.com/p/ontology-browser/wiki/GettingStarted");

    public static final String ACCEPT = "Accept";

    private static URL createURL(String s) {
        try {
            return new URL(s);
        }
        catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }

    // legacy
    public static final String HTML_FRAG = "html-frag";

    // supported formats
    public enum RequestFormat{
        xml(MIME_XML),
        html(MIME_HTML),
        htmlfrag(MIME_HTML);

        private String mimeType;

        RequestFormat(String mimeType) {
            this.mimeType = mimeType;
        }

        public String getMimeType(){
            return mimeType;
        }

        public String getResponseType(){
            return mimeType + ";charset=" + OWLHTMLConstants.DEFAULT_ENCODING;
        }

        // we can't do proper checking because Webkit asks for application/xml first!
        public static RequestFormat get(String mime) {
            int htmlIndex = mime.indexOf(MIME_HTML);
            if (htmlIndex > -1){
                return html;
            }

            int xmlIndex = mime.indexOf(MIME_XML);
            if (xmlIndex > -1){
                return xml;
            }

            return null;
        }
    }

    // and their mime types
    public static final String MIME_XML = "application/xml";
    public static final String MIME_HTML = "text/html";

}

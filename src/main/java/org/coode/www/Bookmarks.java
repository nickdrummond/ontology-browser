package org.coode.www;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.FileUtils;
import org.coode.www.mngr.SessionManager;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.*;
import java.net.URI;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 1, 2010<br><br>
 */
public class Bookmarks {



    public static Map<String, URI> getBookmarks() {
        Map<String, URI> bookmarks = Collections.emptyMap();
//        File bookmarksFile = SessionManager.getFile(OntologyBrowserConstants.BOOKMARKS_XML);
//        if (!bookmarksFile.exists()){
//            FileUtils fileUtils = new FileUtils(".", OWLHTMLConstants.DEFAULT_ENCODING); // path not used
//            InputStream in = Bookmarks.class.getResourceAsStream(OntologyBrowserConstants.DEFAULT_BOOKMARKS_XML);
//            try {
//                fileUtils.saveFile(in, bookmarksFile);
//            }
//            catch (IOException e) {
//                //e.printStackTrace();
//            }
//        }
//
//        try {
//            bookmarks = loadBookmarks(new BufferedReader(new FileReader(bookmarksFile)));
//        }
//        catch (Exception e) {
//            //e.printStackTrace();
//        }
        return bookmarks;
    }

//    private static Map<String, URI> loadBookmarks(Reader reader) throws IOException, SAXException {
//        Map<String, URI> bookmarkMap = new LinkedHashMap<String, URI>();
//        DOMParser parser = new DOMParser();
//        InputSource inputSource = new InputSource(reader);
//        parser.parse(inputSource);
//        Document doc = parser.getDocument();
//        NodeList bookmarkElements = doc.getElementsByTagName("bookmark");
//        for (int i=0; i<bookmarkElements.getLength(); i++){
//            Node element = bookmarkElements.item(i);
//            String name = element.getAttributes().getNamedItem("name").getTextContent();
//            URI uri = URI.create(element.getTextContent());
//            bookmarkMap.put(name, uri);
//        }
//        return bookmarkMap;
//    }
}

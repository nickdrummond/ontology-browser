package org.coode.www.model;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.Resource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class Bookmarks {

    private static Logger logger = LoggerFactory.getLogger(Bookmarks.class);

    private Map<String, URI> bookmarks = Collections.emptyMap();

    public Bookmarks(Resource defaultBookmarksSource) {
        try {
            bookmarks = loadBookmarks(defaultBookmarksSource.getInputStream());
        } catch (Exception e) {
            logger.warn("Could not load bookmarks. ", e);
        }
    }

    public Map<String, URI> getBookmarks() {
        return bookmarks;
    }

    private Map<String, URI> loadBookmarks(InputStream inputStream) throws IOException, SAXException {
        Map<String, URI> bookmarkMap = new LinkedHashMap<String, URI>();
        DOMParser parser = new DOMParser();
        InputSource inputSource = new InputSource(inputStream);
        parser.parse(inputSource);
        Document doc = parser.getDocument();
        NodeList bookmarkElements = doc.getElementsByTagName("bookmark");
        for (int i=0; i<bookmarkElements.getLength(); i++){
            Node element = bookmarkElements.item(i);
            String name = element.getAttributes().getNamedItem("name").getTextContent();
            URI uri = URI.create(element.getTextContent());
            bookmarkMap.put(name, uri);
        }
        return bookmarkMap;
    }


    private void doThing() {
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
    }


}

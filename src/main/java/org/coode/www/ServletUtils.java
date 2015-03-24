package org.coode.www;

import org.coode.html.impl.OWLHTMLParam;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 26, 2010<br><br>
 */
public class ServletUtils {

    // requestURL on its own is not good enough - doesn't include params
    // Need to rebuild the URL
    public static URL getPageURL(HttpServletRequest request) throws IOException {

        // if the request is for an html-frag and we have a referer page we return the url of the referer
        String query = request.getQueryString();
        if (query != null){
            String referer = request.getHeader("Referer");
            if (referer != null){
                for (String param : query.split("&")){
                    if (param.startsWith(OWLHTMLParam.format.toString()) && param.endsWith(OntologyBrowserConstants.HTML_FRAG)){
                        return new URL(referer);
                    }
                }
            }
        }

        StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
        boolean appendedParams = false;

        if (query != null){
            for (String param : query.split("&")){
                if (!param.startsWith(OWLHTMLParam.session.name())){
                    if (appendedParams){
                        requestURL.append("&");
                    }
                    else{
                        requestURL.append("?");
                        appendedParams = true;
                    }
                    requestURL.append(param);
                }
            }
        }
        return new URL(requestURL.toString());
    }


}

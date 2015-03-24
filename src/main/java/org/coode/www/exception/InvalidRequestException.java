/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.exception;

import org.coode.html.impl.OWLHTMLParam;

import javax.servlet.http.HttpServletRequest;
import java.util.Enumeration;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 14, 2008<br><br>
 */
public class InvalidRequestException extends OntServerException {

    public InvalidRequestException(HttpServletRequest request, Map<OWLHTMLParam, Set<String>> requiredParams) {
        super("<h2>INCORRECT USAGE:</h2>" + renderRequest(request) + "<p>should be:<br />" + renderRequiredParams(requiredParams));
    }


    private static String renderRequiredParams(Map<OWLHTMLParam, Set<String>> requiredParams) {
        StringBuilder s = new StringBuilder();
        for (OWLHTMLParam param : requiredParams.keySet()){
            final Set<String> requiredValues = requiredParams.get(param);
            if (requiredValues.size() > 1){
                s.append(param).append("=[");
                for (String value : requiredValues){
                    s.append(value).append("|");
                }
                s.deleteCharAt(s.length()-1);
                s.append("]");
            }
            else{
                s.append("=").append(requiredValues.iterator().next());
            }
            s.append(", ");
        }
        s.deleteCharAt(s.length()-1);
        s.deleteCharAt(s.length()-1);       
        return s.toString();
    }


    private static String renderRequest(HttpServletRequest request) {
        StringBuilder s = new StringBuilder();
//                    s.append("<h3>Request info</h3>");
//            s.append("<br />getCharacterEncoding: " + request.getCharacterEncoding());
//            s.append("<br />getContentLength: " + request.getContentLength());
//            s.append("<br />getContentType: " + request.getContentType());
//            s.append("<br />getProtocol: " + request.getProtocol());
//            s.append("<br />getRemoteAddr: " + request.getRemoteAddr());
//            s.append("<br />getRemoteHost: " + request.getRemoteHost());
//            s.append("<br />getScheme: " + request.getScheme());
//            s.append("<br />getServerName: " + request.getServerName());
//            s.append("<br />getServerPort: " + request.getServerPort());
//            s.append("<br />getAuthType: " + request.getAuthType());
//            s.append("<br />getMethod: " + request.getMethod());
//            s.append("<br />getPathInfo: " + request.getPathInfo());
//            s.append("<br />getPathTranslated: " + request.getPathTranslated());
//            s.append("<br />getQueryString: " + request.getQueryString());
//            s.append("<br />getRemoteUser: " + request.getRemoteUser());
//            s.append("<br />getRequestURI: " + request.getRequestURI());
//            s.append("<br />getServletPath: " + request.getServletPath());

            s.append("<br />Parameters:");
            Enumeration paramNames = request.getParameterNames();
            while (paramNames.hasMoreElements()) {
                String name = (String) paramNames.nextElement();
                String[] values = request.getParameterValues(name);
                s.append("    " + name + ":");
                for (int i = 0; i < values.length; i++) {
                    s.append("<br />      " + values[i]);
                }
            }

//            s.append("<br />Request headers:");
//            Enumeration headerNames = request.getHeaderNames();
//            while (headerNames.hasMoreElements()) {
//                String name = (String) headerNames.nextElement();
//                String value = request.getHeader(name);
//                s.append("  " + name + " : " + value);
//            }
//
//            s.append("<br />Cookies:");
//            Cookie[] cookies = request.getCookies();
//            for (int i = 0; i < cookies.length; i++) {
//                String name = cookies[i].getName();
//                String value = cookies[i].getValue();
//                s.append("<br />  " + name + " : " + value);
//            }
        return s.toString();
//        StringBuilder s = new StringBuilder();
//        s.append"[";
//        final Map map = request.getParameterMap();
//        for (Object param : map.keySet()){
//            for (Object value : (String[])map.get(param)){
//                str += param + ": " + value + ", ";
//            }
//        }
//        str += "]";
//        return str;
    }

//        private final void renderRequestDetails(HttpServletRequest request, HttpServletResponse response){
//        response.setContentType("text/html");
//        try {
//            PrintWriter out = response.getWriter();
//
//            // Print the HTML header
//            out.println("<HTML><HEAD><TITLE>");
//            out.println("Request info");
//            out.println("</TITLE></HEAD>");
//
//            // Print the HTML body
//            out.println("<BODY>");
//            renderRequest(request);
//
//            // Print the HTML footer
//            out.println("</PRE></BODY></HTML>");
//            out.close();
//        }
//        catch (IOException e) {
//            logger.error(e);
//        }
//    }
}

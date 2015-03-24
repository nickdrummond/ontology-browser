package org.coode.www;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 25, 2009<br><br>
 */
public class TestServlet extends HttpServlet {

    private static final String UTF8 = "utf8";

    private PrintStream out;


    public TestServlet() {
        // trying to print out the utf8 - this does not work in intelliJ
        try {
            out = new PrintStream(System.out, true, UTF8);
        }
        catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
    }


    protected final void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("TestServlet.doGet");

        handleRequest(request, response);
    }


    protected final void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("TestServlet.doPost");

        handleRequest(request, response);
    }


    private void handleRequest(HttpServletRequest request, HttpServletResponse response) {

        try {
            // the combination of the response encoding and the content declaration in the head ensures the client displays correctly
            response.setCharacterEncoding(UTF8);
            final PrintWriter writer = response.getWriter(); // can only get this once the encoding set
            writer.write("<html>" +
                                       "<head>" +
                                       "<meta http-equiv='content-type' content='text/html;charset=" + UTF8 + "'>" +
                                       "</head>" +
                                       "<body>");

            String redirect = request.getParameter("redirect");
            String value2 = request.getParameter("value2");

            if (redirect != null){
                // decode the input into utf8
                String value = decode(redirect);
                out.println("redirect = " + redirect);

                // only url encode the value not the whole URL
                value = URLEncoder.encode(value, UTF8);
                response.sendRedirect("?value2=" + value);
            }
            else if (value2 != null){
                System.out.println("value2 = " + value2);
                // decode the input into utf8
                value2 = decode(value2);
                out.println("value2 decoded = " + value2);
                writer.write(value2);
            }
            else{
                writer.write("é (e ecoute directly in java string)<br />");
                writer.write("?? (chinese characters directly in java string)<br />");
                writer.write("%E5%A4%A7%E5%9E%8B (chinese characters % URL escaped)<br />");
                writer.write("<a href='?redirect=??'>redirect to ??</a><br />");
                writer.write("<a href='?redirect=abc'>redirect to abc</a><br />");
                writer.write("<a href='?redirect=%E5%A4%A7%E5%9E%8B'>redirect to ?? (url encoded)</a><br />");
                writer.write("<a href='?value2=??'>value2 ??</a><br />");
                writer.write("<a href='?value2=abc'>value2 abc</a><br />");

                writer.write("<h1>POST redirect form</h1><form method='POST' action='.'>" +
                             "<input type='text' name='redirect' />" +
                             "</form>");

                writer.write("<h1>GET redirect form</h1><form method='GET' action='.'>" +
                             "<input type='text' name='redirect' />" +
                             "</form>");

                writer.write("<h1>POST form</h1><form method='POST' action='.'>" +
                             "<input type='text' name='value2' />" +
                             "</form>");

                writer.write("<h1>GET form</h1><form method='GET' action='.'>" +
                             "<input type='text' name='value2' />" +
                             "</form>");
            }

            writer.write("</body></html>");
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    private String decode(String s) {
        try {
            if (!s.startsWith("%")){
                return new String(s.getBytes("8859_1"), UTF8);
            }
        }
        catch (UnsupportedEncodingException e) {
            out.println("Failed to decode: " + s);
            e.printStackTrace();
        }
        return s;
    }
}

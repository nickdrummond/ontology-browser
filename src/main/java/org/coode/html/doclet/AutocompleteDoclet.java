/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLConstants;
import org.coode.html.url.PermalinkURLScheme;
import org.coode.www.util.URLUtils;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import java.io.PrintWriter;
import java.net.URL;
import java.util.List;
import java.util.Set;

@Deprecated
public class AutocompleteDoclet extends AbstractOWLDocDoclet {

    private static final Logger logger = LoggerFactory.getLogger(AutocompleteDoclet.class);

    private String id;
    private boolean multiword = false;
    private boolean autoSubmitOnAccept;
    private URL submitURL;
    private String initialValue;
    private String method = "get";
    private OWLHTMLConstants.LinkTarget target = null;

    private String paramName = "expression";
    private String submitName = "find";

    private String jsAction; //javascript to be run when page loads (will be contained in a 'delimited string so make sure it contains \" for literals )

    private static final String ID = "doclet.autocomplete";

    private boolean isTextArea = false;


    public AutocompleteDoclet(OWLHTMLKit kit, String id, boolean autoSubmitOnAccept) {
        super(kit);
        this.id = id;
        this.autoSubmitOnAccept = autoSubmitOnAccept;
    }

    public void setSubmitURL(URL submitURL){
        this.submitURL = submitURL;
    }

    public void setSubmitName(String submitName) {
        this.submitName = submitName;
    }

    public void setIsTextArea(boolean isTextArea){
        this.isTextArea = isTextArea;
    }

    public void setJsAction(String jsAction) {
        this.jsAction = jsAction;
    }

    public void setParamName(String paramName) {
        this.paramName = paramName;
    }

    public void setMultiword(boolean multiword) {
        this.multiword = multiword;
    }

    public void setInitialValue(String initialValue) {
        this.initialValue = initialValue;
    }

    public void setTarget(OWLHTMLConstants.LinkTarget target) {
        this.target = target;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        String actionStr = "";
        if (submitURL != null){
            actionStr = URLUtils.createRelativeURL(pageURL, submitURL);
        }

        out.print("\n<form class='autocomplete' method='");
        out.print(method);
//        out.print("' id='chooser");
//        out.print("' accept-charset='");
//        out.print(OWLHTMLConstants.DEFAULT_ENCODING);
        out.print("' id='");
        out.print(id);
        out.print("Form' action='");
        out.print(actionStr);
        out.print("'");
        if (!isSingleFrameNavigation() && target != null){
            out.print(" target='" + target + "'");
        }
        out.println(">");

        if (isTextArea){
            out.print("<textarea name='" + paramName + "' ");
            out.print(" id='" + id + "'>");
            if (initialValue != null && initialValue.length() > 0){
                out.print(initialValue);
            }
            out.println("</textarea>");
        }
        else{
            out.print("<input type='text' name='" + paramName + "' ");
            if (initialValue != null && initialValue.length() > 0){
                out.println(" value='" + initialValue + "'");
            }
            out.println(" id='" + id + "'/>");
        }

        // for storing the selection URI
        out.println("<input name='uri' type='hidden' value='' />");

        out.println("<input name='syntax' id='dlQuerySyntax' type='hidden' value='man' />"); // no harm leaving this in both
        if (getOWLHTMLKit().getCurrentLabel() != null){
            out.println("<input name='session' type='hidden' value='" + getOWLHTMLKit().getCurrentLabel() + "' />");
        }

        if (jsAction != null){
            out.println("<input type='button' value='" + submitName + "' onmouseup='" + jsAction + "' />");
        }
        else{
            out.println("<input type='submit' value='" + submitName + "' />");
        }

        out.println("</form>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {

        String findURL = URLUtils.createRelativeURL(pageURL, new PermalinkURLScheme(getOWLHTMLKit().getURLScheme()).getURLForRelativePage("find/?format=xml&type=entities")) + "&";

        out.print("<script type=\"text/javascript\">\n" +
                  "    var options = {\n" +
                  "        script: \"" + findURL + "\",\n" +
                  "        varname: \"input\",\n" +
                  "        cache: false");
        if (multiword){
            out.println(",\n    multiword: true");
        }
        if (autoSubmitOnAccept){
            out.println(",\n        callback: function (obj){");
            out.println("$(\"#" + id + "Form input[name=uri]\").val(obj.id);"); // set the URI to that of the object
            out.println("$(\"#" + id + "Form\").submit();");                          // submit the form
            out.println("}\n");
        }
        out.println("    };\n" +
                    "    var as = new AutoSuggest(\"" + id + "\", options);\n" +
                    "</script>");
    }

    public String getID() {
        return ID;
    }

    public Set<URL> getRequiredCSS() {
        Set<URL> css = super.getRequiredCSS();
        css.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.AUTO_SUGGEST_CSS));
        return css;
    }

    public List<URL> getRequiredJS() {
        List<URL> js = super.getRequiredJS();
        js.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.AUTO_SUGGEST_JS));
        return js;
    }
}

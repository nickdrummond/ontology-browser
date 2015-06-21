package org.coode.www.kit.impl;

import org.coode.html.url.RestURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.coode.owl.mngr.impl.ServerOptionsAdapterImpl;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.*;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private URL baseURL;

    protected URLScheme urlScheme;

    private String label;

    private OWLServer owlServer;

    private ServerOptionsAdapter<OWLHTMLProperty> properties;

    private String id;

    private List<String> errorMessages = new ArrayList<String>();

    private Date timestamp;


    public OWLHTMLKitImpl(URL baseURL) {
        this(new OWLServerImpl(OWLManager.createOWLOntologyManager()), baseURL);
    }

    public OWLHTMLKitImpl(OWLServer server, URL baseURL) {
        this.timestamp = new Date(System.currentTimeMillis());
        this.id = createID();
        this.owlServer = server;
        this.baseURL = baseURL;
    }

    private String createID() {
        return Long.toHexString(System.currentTimeMillis());
    }

    public ServerOptionsAdapter<OWLHTMLProperty> getHTMLProperties() {
        if (properties == null){
            // share the same base properties
            properties = new ServerOptionsAdapterImpl<OWLHTMLProperty>((ServerOptionsAdapterImpl)getOWLServer().getProperties());
            properties.addDeprecatedNames(OWLHTMLProperty.generateDeprecatedNamesMap());

            properties.setBoolean(OWLHTMLProperty.optionShowInferences, true);

            // Allowed values
            List<String> booleanValues = Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString());
            properties.setAllowedValues(OWLHTMLProperty.optionRenderPermalink, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionShowMiniHierarchies, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionShowInferredHierarchies, booleanValues);
            properties.setAllowedValues(OWLHTMLProperty.optionShowInferences, booleanValues);
        }
        return properties;
    }

    public String getID() {
        return id;
    }


    public OWLServer getOWLServer() {
        return owlServer;
    }


    public URL getBaseURL(){
        return baseURL;
    }

    public URLScheme getURLScheme() {
        if (urlScheme == null && !owlServer.isDead()){
            urlScheme = new RestURLScheme(this);
        }
        return urlScheme;
    }

    public void setURLScheme(URLScheme urlScheme) {
        this.urlScheme = urlScheme;
    }

    public Set<OWLOntology> getVisibleOntologies() {
        return owlServer.getActiveOntologies();
    }

    public void setCurrentLabel(String label) {
        this.label = label;
    }

    public String getCurrentLabel() {
        return label;
    }


    public void dispose() {
        owlServer.dispose();
        properties = null;
        urlScheme = null;
        baseURL = null;
        label = null;
    }

    public boolean isActive() {
        return !owlServer.isDead();
    }

    public void addUserError(String errorMessage) {
        errorMessages.add(errorMessage);
    }

    public void addUserError(String errorMessage, Throwable error) {
        Throwable cause = error;
        while (cause.getCause() != null){
            cause = cause.getCause();
        }
        String msg = cause.getMessage();
        if (msg == null){
            StringWriter stringWriter = new StringWriter();
            final PrintWriter printWriter = new PrintWriter(stringWriter);
            cause.printStackTrace(printWriter);
            printWriter.flush();
            msg = stringWriter.toString();
        }
        errorMessages.add("<p>" + errorMessage + "</p>" + msg);

    }

    public List<String> getUserErrors() {
        return new ArrayList<String>(errorMessages);
    }

    public void clearUserErrors() {
        errorMessages.clear();
    }

    @Override
    public String toString() {
        return "OWLHTMLKitImpl{" +
               "id='" + id + '\'' +
               ", timestamp=" + timestamp +
               '}';
    }
}

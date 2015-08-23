package org.coode.www.kit.impl;

import org.coode.html.url.RestURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.impl.OWLServerImpl;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.ServerConfig;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;

import javax.annotation.Nonnull;
import java.net.URL;
import java.util.Set;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private URL baseURL;

    protected URLScheme urlScheme;

    private OWLServer owlServer;

    private ServerConfig config;

    public OWLHTMLKitImpl(URL baseURL) {
        this(new OWLServerImpl(OWLManager.createOWLOntologyManager()), baseURL);
    }

    public OWLHTMLKitImpl(OWLServer server, URL baseURL) {
        this.owlServer = server;
        this.baseURL = baseURL;
        this.config = new ServerConfig();
        this.owlServer.setConfig(config);
    }

    @Override
    public OWLServer getOWLServer() {
        return owlServer;
    }

    @Override
    public ServerConfig getConfig() {
        return config;
    }

    @Override
    public void setConfig(@Nonnull ServerConfig serverConfig) {
        this.config = serverConfig;
        this.owlServer.setConfig(serverConfig);
    }

    @Override
    public URL getBaseURL(){
        return baseURL;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme(this);
        }
        return urlScheme;
    }

    @Override
    public Set<OWLOntology> getVisibleOntologies() {
        return owlServer.getActiveOntologies();
    }

    @Override
    public String getCurrentLabel() {
        String ontHash = getOntConfig().getHash();
        String servHash = config.getHash();
        return ontHash + "_" + servHash;
    }

    @Override
    public void dispose() {
        owlServer.dispose();
        urlScheme = null;
        baseURL = null;
    }

    @Override
    public boolean isActive() {
        return !owlServer.isDead();
    }

    @Override
    public OntologyConfig getOntConfig() {
        return OntologyConfig.ontConfigFor(owlServer.getActiveOntology());
    }
}

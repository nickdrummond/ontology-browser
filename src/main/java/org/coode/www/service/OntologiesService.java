package org.coode.www.service;

import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.mngr.KitRepository;
import org.semanticweb.owlapi.io.UnparsableOntologyException;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Service
public class OntologiesService {

    @Autowired
    private KitRepository kitRepository;

    public OWLOntology getOntologyFor(final String id, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOWLServer().getOntologies()){
            if (getIdFor(ont).equals(id)){
                return ont;
            }
        }
        throw new NotFoundException("Ontology", id);
    }

    public String getIdFor(final OWLOntology ontology) {
        return String.valueOf(ontology.getOntologyID().hashCode());
    }

    /**
     * @return the ID of the active ontology
     */
    public String load(URI uri, boolean clear, OWLHTMLKit kit) throws OntServerException {

        Map<URI, Throwable> fail = new HashMap<URI, Throwable>();

        OWLServer server = kit.getOWLServer();

        if (clear) {
            server.clearOntologies();
        }

        try {
            if (uri.isAbsolute()) {
                OWLOntology ont = server.loadOntology(uri);
                kitRepository.saveKit(kit);
                return String.valueOf(ont.hashCode());
            }
            else {
                throw new IllegalArgumentException("Ontology URIs must be absolute: " + uri);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
            fail.put(uri, e);
        }
        catch (OutOfMemoryError e) {
            fail.put(uri, e);
            // clear all ontologies as we are in an unpredictable state
            server.clearOntologies();
            throw new OntServerException("Out of memory trying to load ontologies");
        }

        for (URI f : fail.keySet()) {
            String message;
            if (fail.get(f) instanceof UnparsableOntologyException) {
                message = "Maybe it is not an ontology/linked data file.";
            } else {
                message = fail.get(f).getMessage();
            }
            kit.addUserError("<p>Failed to load: " + uri + "</p><p>" + message + "</p>");
        }
        return String.valueOf(kit.getOWLServer().getActiveOntology().hashCode());
    }

    public OWLOntology getActiveOntology(final OWLHTMLKit kit) {
        return kit.getOWLServer().getActiveOntology();
    }

    public Set<OWLOntology> getOntologies(final OWLHTMLKit kit) {
        return kit.getOWLServer().getOntologies();
    }

    public HierarchyProvider<OWLOntology> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLOntology.class);
    }
}

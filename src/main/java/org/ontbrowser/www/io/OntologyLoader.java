package org.ontbrowser.www.io;

import org.ontbrowser.www.kit.impl.BaseURIMapper;
import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.io.File;
import java.io.IOException;
import java.net.URI;

public class OntologyLoader {

    private final OWLOntologyManager mngr;

    private static final Logger logger = LoggerFactory.getLogger(OntologyLoader.class);

    public OntologyLoader() {
        mngr = OWLManager.createOWLOntologyManager();
    }

    public OWLOntology loadOntologies(String location) throws OWLOntologyCreationException {

        // check if already loaded
        IRI iri = IRI.create(location);
        for (OWLOntology ont : mngr.getOntologies()) {
            if (mngr.getOntologyDocumentIRI(ont).equals(iri)) {
                return ont;
            }
        }

        OWLOntologyLoaderListener ontLoadListener = new OWLOntologyLoaderListener() {

            private int count = 0;
            public void startedLoadingOntology(@NonNull LoadingStartedEvent loadingStartedEvent) {
                // do nothing
            }

            public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
                count++;
                if (loadingFinishedEvent.isSuccessful() && !loadingFinishedEvent.isImported()) {
                    OWLOntologyID id = loadingFinishedEvent.getOntologyID();
                    logger.info("Loaded {} with {} imports", OWLUtils.ontIRI(id), count-1);
                }
            }
        };
        mngr.addOntologyLoaderListener(ontLoadListener);

        if (location.startsWith("/")) { // absolute file path
            var physicalURI = "file://" + location;
            addBaseMapperFor(physicalURI);
            return mngr.loadOntologyFromOntologyDocument(IRI.create(physicalURI));
        }
        else if (location.contains("://")) { // Looks like a URL
            addBaseMapperFor(location);
            return mngr.loadOntologyFromOntologyDocument(iri);
        }
        else { // relative file path?
            File file = new File(location);
            if (file.exists()) {
                var physicalURI = "file://" + file.getAbsolutePath();
                addBaseMapperFor(physicalURI);
                logger.info("Loading ontology from {}", physicalURI);
                return mngr.loadOntologyFromOntologyDocument(IRI.create(physicalURI));
            }
            else { // Classpath?
                var res = getClass().getClassLoader().getResource(location);
                logger.info("Loading ontology from {}", res);
                try(var inputStream = res.openStream()) {
                    if (inputStream == null) {
                        throw new RuntimeException("Could not open ontology from " + location);
                    }
                    return mngr.loadOntologyFromOntologyDocument(inputStream);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    // using the location of explicitly loaded ontologies as a hint for loading imports
    private void addBaseMapperFor(String physicalURI) {
        URI baseURI = URI.create(physicalURI.substring(0, physicalURI.lastIndexOf("/")+1));
        logger.info("Loading imports from {}", baseURI);
        mngr.getIRIMappers().add(new BaseURIMapper(baseURI));
    }
}

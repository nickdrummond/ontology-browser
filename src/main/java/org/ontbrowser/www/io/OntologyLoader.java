package org.ontbrowser.www.io;

import com.google.common.collect.Maps;
import org.ontbrowser.www.kit.impl.BaseURIMapper;
import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.HashSet;
import java.util.Map;

public class OntologyLoader {

    private OWLOntologyManager mngr;

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

        if (location.startsWith("/")) {
            handleCommonBaseMappers(mngr, URI.create("file://" + location));
            try (InputStream ontAsStream = new FileInputStream(location)) {
                return mngr.loadOntologyFromOntologyDocument(ontAsStream);
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Cannot load: " + location);
            } catch (IOException e) {
                throw new RuntimeException("Cannot load: " + location);
            }
        }
        else {
            handleCommonBaseMappers(mngr, URI.create(location));
            return mngr.loadOntologyFromOntologyDocument(iri);
        }
    }

    // create a set of CommonBaseURIMappers for finding ontologies
    // using the base of explicitly loaded ontologies as a hint
    private void handleCommonBaseMappers(OWLOntologyManager mngr, URI physicalURI) {
        final StringBuilder baseURIStr = new StringBuilder();
        final String[] uriParts = physicalURI.toString().split("/");
        for (int i=0; i<uriParts.length-1; i++){
            baseURIStr.append(uriParts[i]).append("/");
        }
        URI baseURI = URI.create(baseURIStr.toString());

        final Map<URI, OWLOntologyIRIMapper> baseMapper = Maps.newHashMap();
        final BaseURIMapper mapper = new BaseURIMapper(baseURI);
        baseMapper.put(baseURI, mapper);
        mngr.setIRIMappers(new HashSet<>(baseMapper.values()));
    }
}

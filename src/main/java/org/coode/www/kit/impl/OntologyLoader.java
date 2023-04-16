package org.coode.www.kit.impl;

import com.google.common.collect.Maps;
import org.coode.owl.mngr.impl.BaseURIMapper;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.io.InputStream;
import java.net.URI;
import java.util.HashSet;
import java.util.Map;

public class OntologyLoader {

    private static final Logger logger = LoggerFactory.getLogger(OntologyLoader.class);

    public OWLOntology loadOntologies(OWLOntologyManager mngr, URI physicalURI) throws OWLOntologyCreationException {

        // check if already loaded
        IRI iri = IRI.create(physicalURI);
        for (OWLOntology ont : mngr.getOntologies()) {
            if (mngr.getOntologyDocumentIRI(ont).equals(iri)) {
                return ont;
            }
        }

        OWLOntologyLoaderListener ontLoadListener = new OWLOntologyLoaderListener() {
            public void startedLoadingOntology(@NonNull LoadingStartedEvent loadingStartedEvent) {
                // do nothing
            }

            public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
                if (loadingFinishedEvent.isSuccessful() && !loadingFinishedEvent.isImported()) {
                    OWLOntologyID id = loadingFinishedEvent.getOntologyID();
                    logger.info("loaded " +id.getDefaultDocumentIRI().map(IRI::toString));
                }
            }
        };
        mngr.addOntologyLoaderListener(ontLoadListener);

        handleCommonBaseMappers(mngr, physicalURI);

        if (!physicalURI.isAbsolute()) {
            ClassLoader classLoader = getClass().getClassLoader();
            InputStream ontAsStream = classLoader.getResourceAsStream(physicalURI.getPath());
            if (ontAsStream == null) {
                throw new RuntimeException("Cannot load: " + physicalURI + " from classpath: " + classLoader.getResource("/"));
            }
            return mngr.loadOntologyFromOntologyDocument(ontAsStream);
        }
        else {
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

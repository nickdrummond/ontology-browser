package org.coode.www.repository;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.ServerConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Repository;

import java.io.IOException;
import java.net.URL;

@Repository
public class KitRepository {

    @Autowired
    private ApplicationContext appCtx;

    private static Logger logger = LoggerFactory.getLogger(KitRepository.class);

    @Autowired
    private OntologyConfigRepo ontologyConfigRepo;

    @Autowired
    private ServerConfigRepo serverConfigRepo;

    private static final String LABEL_SPLITTER = "_";

    /**
     * Persist the current loaded ontologies with their mappings.
     * @param kit note: will set the current label on the server
     * @throws org.coode.www.exception.OntServerException
     */
    public synchronized void saveKit(final OWLHTMLKit kit) throws OntServerException {
        try {
            saveServerConfig(kit.getConfig());
            saveOntologies(kit.getOntConfig());
        }
        catch (IOException e) {
            throw new OntServerException(e);
        }
    }

    /**
     * Clears the given server and replaces its state with that specified by the label given
     */
    public synchronized void loadKit(OWLHTMLKit kit, String label) throws OntServerException {

        logger.info("Loading kit for label: " + label);

        String[] parts = label.split(LABEL_SPLITTER);
        String ontHash = parts[0];
        String servHash = parts[1];

        kit.loadOntologies(ontologyConfigRepo.findByHash(ontHash));
        kit.setConfig(serverConfigRepo.findByHash(servHash));
    }

    public OWLHTMLKit createHTMLKit(URL basePath) {
        final OWLHTMLKit kit = appCtx.getBean(OWLHTMLKit.class);
        kit.setBaseUrl(basePath);
        return kit;
    }

    private ServerConfig saveServerConfig(ServerConfig config) throws IOException {
        try {
            config = serverConfigRepo.save(config);
            logger.info("Saved server config");
            return config;
        }
        catch(DuplicateKeyException e) {
            logger.info("Already saved server config");
            return config;
        }
    }

    private OntologyConfig saveOntologies(OntologyConfig config) throws IOException {
        try {
            config = ontologyConfigRepo.save(config);
            logger.info("Saved ontologies config");
            return config;
        }
        catch(DuplicateKeyException e) {
            logger.info("Already saved ontologies config");
            return config;
        }
    }
}

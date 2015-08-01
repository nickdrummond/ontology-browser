package org.coode.www.mngr;

import com.google.common.base.Function;
import com.google.common.collect.Lists;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.OntologyMapping;
import org.coode.www.repository.OntologyConfigRepo;
import org.coode.www.util.Hashing;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import javax.annotation.Nullable;
import java.io.*;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

@Repository
public class KitRepository {

    private static Logger logger = LoggerFactory.getLogger(KitRepository.class);

    @Value("${application.version}")
    private String appVersion;

    @Value("${cache.location}")
    private String cacheLocation;

    private OntologyConfigRepo ontologyConfigRepo = null; // TODO Autowire

    private static final String PROPERTIES_PREFIX = "properties.";
    private static final String PROPERTIES_EXT = ".xml";
    private static final String LABEL_SPLITTER = "_";

    /**
     * Persist the current loaded ontologies with their mappings
     * @param kit note: will set the current label on the server
     * @throws org.coode.www.exception.OntServerException
     */
    public synchronized void saveKit(final OWLHTMLKit kit) throws OntServerException {
        try {
            String propLabel = saveProperties(kit);

            OntologyConfig ontConfig = saveOntologies(kit.getOWLServer().getActiveOntology());

            kit.setCurrentLabel(ontConfig.getHash() + LABEL_SPLITTER + propLabel);
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

        try{
            OntologyConfig ontConfig = loadOntologies(ontHash);
            kit.getOWLServer().loadOntologies(ontConfig);

            loadProperties(parts[1], kit);

            kit.setCurrentLabel(label);
        }
        catch(IOException e){
            throw new OntServerException(e);
        }
    }

    public OWLHTMLKit createHTMLKit(URL basePath) {

        OWLHTMLKit kit = new OWLHTMLKitImpl(basePath);

        // TODO remove me? Not available in OWLAPI 2014
//        // set silent error handling for missing imports
//        kit.getOWLServer().getOWLOntologyManager().setSilentMissingImportsHandling(true);

        boolean defaultsLoaded = false;

        // we will likely want different defaults for different versions (or run versions on the same server)
        File file = getFile("default" + appVersion + PROPERTIES_EXT);

        if (file.exists()){
            try {
                BufferedInputStream in = new BufferedInputStream(new FileInputStream(file));
                kit.getHTMLProperties().load(in);
                in.close();
                defaultsLoaded = true;
            }
            catch (IOException e) {
                logger.error("Could not load default properties");
            }
        }

        if (!defaultsLoaded){

            setupDefaultServerProperties(kit);

            try {
                OutputStream out = new FileOutputStream(file);
                kit.getHTMLProperties().save(out);
            }
            catch (IOException e) {
                logger.error("Could not save default properties");
            }
        }

        return kit;
    }

    private OntologyConfig saveOntologies(OWLOntology activeOnt) throws IOException {
        logger.info("Saving ontologies");

        List<OntologyMapping> mappings = Lists.newArrayList();
        mappings.add(mappingFor(activeOnt));
        Stream<OWLOntology> allOnts = activeOnt.getImportsClosure().stream();
        allOnts.filter(ont -> !ont.equals(activeOnt)).forEach(ont -> mappings.add(mappingFor(ont)));
        OntologyConfig config = new OntologyConfig(mappings);

        return ontologyConfigRepo.save(config);
    }

    private OntologyMapping mappingFor(OWLOntology ont) {
        IRI docIRI = ont.getOWLOntologyManager().getOntologyDocumentIRI(ont);
        IRI ontIRI = ont.getOntologyID().getDefaultDocumentIRI().or(docIRI);
        return new OntologyMapping(ontIRI, docIRI);
    }

    private String saveProperties(OWLHTMLKit kit) throws IOException {
        logger.info("Saving properties");

        ByteArrayOutputStream out = new ByteArrayOutputStream(3 * 1024); // < 3k
        kit.getHTMLProperties().save(out);
        out.flush();
        out.close();
        byte[] bytes = out.toByteArray();
        String propLabel = Hashing.md5(bytes);
        save(bytes, getCacheFile(propLabel));
        return propLabel;
    }

    private String getCacheFile(String propLabel) {
        return PROPERTIES_PREFIX + propLabel + PROPERTIES_EXT;
    }

    private void save(byte[] bytes, String filename) throws IOException {
        File file = getFile(filename);
        if (!file.exists()) {
            FileOutputStream fOut = new FileOutputStream(file);
            fOut.write(bytes);
            fOut.flush();
            fOut.close();
            logger.info("kit state saved at: " + file.getAbsolutePath());
        }
        else {
            logger.info("kit state already saved at: " + file.getAbsolutePath());
        }
    }

    private void loadProperties(final String label, OWLHTMLKit kit) throws IOException {
        File propsFile = getFile(getCacheFile(label));

        if (!propsFile.exists()){
            throw new FileNotFoundException("Cannot find stored properties: " + propsFile.getAbsolutePath());
        }

        BufferedInputStream in = new BufferedInputStream(new FileInputStream(propsFile));
        kit.getHTMLProperties().load(in);
        in.close();
    }

    private OntologyConfig loadOntologies(final String label) throws IOException {
        return ontologyConfigRepo.findByHash(label);
    }

    private File getFile(String name) {
        File cacheDir = new File(cacheLocation);
        if (!cacheDir.exists()){
            cacheDir.mkdir();
        }
        return new File(cacheLocation + name);
    }

    private void setupDefaultServerProperties(OWLHTMLKit kit) {

        // make sure the reasoner is enabled to allow dl query etc
        kit.getOWLServer().getProperties().setBoolean(ServerProperty.optionReasonerEnabled, true);

        ServerOptionsAdapter<OWLHTMLProperty> properties = kit.getHTMLProperties();

        // render a permalink
        properties.setBoolean(OWLHTMLProperty.optionRenderPermalink, true);

        properties.setBoolean(OWLHTMLProperty.optionShowMiniHierarchies, true);

        properties.setBoolean(OWLHTMLProperty.optionShowInferredHierarchies, false);
    }
}

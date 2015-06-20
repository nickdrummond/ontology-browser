package org.coode.www.mngr;

import com.google.common.base.Function;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import javax.annotation.Nullable;
import java.io.*;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Repository
public class KitRepository {

    private static Logger logger = LoggerFactory.getLogger(KitRepository.class);

    @Value("${application.version}")
    private String appVersion;

    @Value("${cache.location}")
    private String cacheLocation;

    private static final String PROPERTIES_PREFIX = "properties.";
    private static final String ONTOLOGIES_PREFIX = "ontologies.";
    private static final String PROPERTIES_EXT = ".xml";
    private static final String ONTOLOGIES_EXT = ".properties";
    private static final String LABEL_SPLITTER = "_";

    /**
     * Persist the current loaded ontologies with their mappings
     * @param kit note: will set the current label on the server
     * @throws org.coode.www.exception.OntServerException
     */
    public synchronized void saveKit(final OWLHTMLKit kit) throws OntServerException {
        try {
            String propLabel = saveProperties(kit);

            String ontsLabel = saveOntologies(kit);

            kit.setCurrentLabel(ontsLabel + LABEL_SPLITTER + propLabel);
        }
        catch (IOException e) {
            throw new OntServerException(e);
        }
    }


    /**
     * Clears the given server and replaces its state with that specified by the label given
     * @param kit
     * @param label
     * @throws OntServerException
     */
    public synchronized void loadKit(OWLHTMLKit kit, String label) throws OntServerException {

        logger.info("Loading kit for label: " + label);

        String[] parts = label.split(LABEL_SPLITTER);

        try{
            Map<IRI, IRI> ontMap = loadOntologies(parts[0]);
            kit.getOWLServer().loadOntologies(ontMap);

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

    private String saveOntologies(OWLHTMLKit kit) throws IOException {
        logger.info("Saving ontologies");

        ByteArrayOutputStream out = new ByteArrayOutputStream(3 * 1024); // < 3k
        PrintWriter writer = new PrintWriter(out);

        // always print the active ontology first
        OWLOntology activeOnt = kit.getOWLServer().getActiveOntology();
        writer.println(getOntologyIdString(activeOnt) + "=" +
                kit.getOWLServer().getOWLOntologyManager().getOntologyDocumentIRI(activeOnt));

        for (OWLOntology ont : kit.getOWLServer().getOntologies()){
            if (!ont.equals(activeOnt)){
                writer.println(getOntologyIdString(ont) + "=" +
                        kit.getOWLServer().getOWLOntologyManager().getOntologyDocumentIRI(ont));
            }
        }
        writer.flush();
        writer.close();

        byte[] bytes = out.toByteArray();
        String ontsLabel = md5(bytes);
        save(bytes, ONTOLOGIES_PREFIX + ontsLabel + ONTOLOGIES_EXT);
        return ontsLabel;
    }

    private String getOntologyIdString(final OWLOntology ont){
        return ont.getOntologyID().getDefaultDocumentIRI().transform(new Function<IRI, String>(){

            @Nullable
            @Override
            public String apply(IRI iri) {
                return iri.toString();
            }
        }).or(ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString());
    }

    private String saveProperties(OWLHTMLKit kit) throws IOException {
        logger.info("Saving properties");

        ByteArrayOutputStream out = new ByteArrayOutputStream(3 * 1024); // < 3k
        kit.getHTMLProperties().save(out);
        out.flush();
        out.close();
        byte[] bytes = out.toByteArray();
        String propLabel = md5(bytes);
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

    private String md5(byte[] bytes) {
        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            byte[] hashedBytes = digest.digest(bytes);
            return convertByteArrayToHexString(hashedBytes);
        }
        catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }


    private String convertByteArrayToHexString(byte[] arrayBytes) {
        StringBuffer stringBuffer = new StringBuffer();
        for (int i = 0; i < arrayBytes.length; i++) {
            stringBuffer.append(Integer.toString((arrayBytes[i] & 0xff) + 0x100, 16)
                    .substring(1));
        }
        return stringBuffer.toString();
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

    private Map<IRI, IRI> loadOntologies(final String label) throws IOException {

        File ontsFile = getFile(ONTOLOGIES_PREFIX + label + ONTOLOGIES_EXT);

        if (!ontsFile.exists()){
            throw new FileNotFoundException("Cannot find stored ontologies: " + ontsFile.getAbsolutePath());
        }

        BufferedReader reader = new BufferedReader(new FileReader(ontsFile));
        String line;
        Map<IRI, IRI> ontMap = new HashMap<IRI, IRI>();
        while ((line = reader.readLine()) != null){
            String[] param = line.split("=");
            IRI ontURI = IRI.create(param[0].trim());
            final String str = param[1].trim();
            IRI physicalURI = null;
            // protect ourselves against http://a.com=null as null will be a valid relative IRI
            if (!str.equals("null")){
                physicalURI = IRI.create(str);
            }
            ontMap.put(ontURI, physicalURI);
        }
        reader.close();
        return ontMap;
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

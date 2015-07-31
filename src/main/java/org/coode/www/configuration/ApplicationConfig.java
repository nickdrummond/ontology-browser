package org.coode.www.configuration;

import com.mongodb.Mongo;
import com.mongodb.MongoClient;
import org.coode.www.mngr.KitRepository;
import org.coode.www.mngr.SessionManager;
import org.coode.www.model.ApplicationInfo;
import org.coode.www.model.Bookmarks;
import org.coode.www.model.OntologyConfig;
import org.coode.www.service.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.config.AbstractMongoConfiguration;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.index.Index;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.view.InternalResourceViewResolver;
import org.springframework.web.servlet.view.JstlView;

import java.util.Arrays;
import java.util.List;

@Configuration
@ComponentScan("org.coode.www.controller")
@EnableWebMvc
@EnableMongoRepositories
public class ApplicationConfig extends AbstractMongoConfiguration {

    @Bean
    public InternalResourceViewResolver setupViewResolver() {
        InternalResourceViewResolver resolver = new InternalResourceViewResolver();
        resolver.setPrefix("/WEB-INF/views/");
        resolver.setSuffix(".jsp");
        resolver.setViewClass(JstlView.class);
        return resolver;
    }

    @Bean
    public ApplicationInfo applicationInfo(@Value("${application.name}") String applicationName,
                                           @Value("${application.version}") String applicationVersion,
                                           @Value("${application.url}") String applicationUrl) {
        return new ApplicationInfo(applicationName, applicationVersion, applicationUrl);
    }

    @Bean
    public OntologyIRIShortFormProvider ontologyIRIShortFormProvider() {
        return new OntologyIRIShortFormProvider();
    }

    @Bean
    public GeoService geoService(@Value("${geo.latitude}") String lat,
                                 @Value("${geo.longitude}") String longitude,
                                 @Value("${geo.point}") String point) {
        return new GeoService(lat, longitude, point);
    }

    @Bean
    public MediaService mediaService() {
        return new MediaService();
    }

    @Bean
    public NameService nameService() {
        return new NameService();
    }

    @Bean
    public SearchService searchService() {
        return new SearchService();
    }

    @Bean
    public ParserService parserService() {
        return new ParserService();
    }

    @Bean
    public ReasonerService reasonerService() {
        return new ReasonerService();
    }

    @Bean
    public OntologiesService ontologiesService() {
        return new OntologiesService();
    }

    @Bean
    public OWLClassesService owlClassesService() {
        return new OWLClassesService();
    }

    @Bean
    public OWLObjectPropertiesService owlObjectPropertiesService() {
        return new OWLObjectPropertiesService();
    }

    @Bean
    public OWLDataPropertiesService owlDataPropertiesService() {
        return new OWLDataPropertiesService();
    }

    @Bean
    public OWLAnnotationPropertiesService owlAnnotationPropertiesService() {
        return new OWLAnnotationPropertiesService();
    }

    @Bean
    public OWLDatatypesService owlDatatypesService() {
        return new OWLDatatypesService();
    }

    @Bean
    public OWLIndividualsService owlIndividualsService() {
        return new OWLIndividualsService();
    }

    @Bean
    public Bookmarks bookmarks(@Value("${bookmarks.source}") String bookmarksSource) {
        return new Bookmarks(new ClassPathResource(bookmarksSource));
    }

    @Bean
    public OptionsService optionsService() {
        return new OptionsService();
    }

    @Bean
    public ReasonerFactoryService reasonerFactoryService() {
        // TODO Springify the factory class names?
        List<String> reasonerFactoryNames = Arrays.asList(
                "org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory",
                "uk.ac.manchester.cs.jfact.JFactFactory",
                "org.semanticweb.HermiT.Reasoner$ReasonerFactory",
                "org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory",
                "uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory",
                "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory"
        );
        return new ReasonerFactoryService(reasonerFactoryNames);
    }

    @Bean
    public KitRepository kitRepository() {
        return new KitRepository();
    }

    @Bean
    public SessionManager sessionManager() {
        return new SessionManager();
    }

    @Override
    protected String getDatabaseName() {
        return "ontology-browser";
    }

    @Value("${mongo.host}")
    private String mongoHost;

    @Value("${mongo.port}")
    private int mongoPort;

    @Override
    public Mongo mongo() throws Exception {
        return new MongoClient(mongoHost, mongoPort);
    }

    @Override
    public MongoTemplate mongoTemplate() throws Exception {
        MongoTemplate template = super.mongoTemplate();
        template.indexOps(OntologyConfig.class).ensureIndex(new Index().on("hash", Sort.Direction.ASC));
        return template;
    }

    @Override
    protected String getMappingBasePackage() {
        return "org.coode.www.repository";
    }
}
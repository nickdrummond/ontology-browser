package org.coode.www.repository;

import com.github.fakemongo.Fongo;
import com.google.common.collect.Lists;
import com.mongodb.Mongo;
import com.mongodb.MongoClient;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.OntologyMapping;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.config.AbstractMongoConfiguration;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import java.net.UnknownHostException;
import java.util.List;

import static org.junit.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(loader=AnnotationConfigContextLoader.class)
@ActiveProfiles("test")
public class OntologyConfigRepoTest {

    @Configuration
    @EnableMongoRepositories
    static class ContextConfiguration extends AbstractMongoConfiguration {

        @Override
        protected String getDatabaseName() {
            return "test";
        }

        @Override
        public Mongo mongo() throws UnknownHostException {
            return new Fongo("Fake Mongo").getMongo();
//          return new MongoClient("localhost", 27017); // use real Mongo for integration tests
        }

        @Override
        protected String getMappingBasePackage() {
            return "org.coode.www.repository";
        }
    }

    @Autowired
    OntologyConfigRepo repository;

    @Test
    public void roundtrip() {

        List<OntologyMapping> mappings = Lists.newArrayList(
                new OntologyMapping(IRI.create("http://example.com"), IRI.create("http://example.com/location")),
                new OntologyMapping(IRI.create("http://example2.com"), IRI.create("http://example2.com/location"))
        );
        OntologyConfig saved = new OntologyConfig(mappings);

        repository.save(saved);

        OntologyConfig loaded = repository.findByHash(saved.getHash());
        assertEquals(saved, loaded);
    }
}

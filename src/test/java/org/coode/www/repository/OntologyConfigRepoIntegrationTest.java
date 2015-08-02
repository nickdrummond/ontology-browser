package org.coode.www.repository;

import com.github.fakemongo.Fongo;
import com.google.common.collect.Lists;
import com.mongodb.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.BsonDocument;
import org.bson.BsonString;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.coode.www.configuration.MongoConfiguration;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.OntologyMapping;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.AnnotationConfigContextLoader;

import java.net.UnknownHostException;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(loader=AnnotationConfigContextLoader.class)
@ActiveProfiles("test")
public class OntologyConfigRepoIntegrationTest {

    @Configuration
    static class TestConfiguration extends MongoConfiguration {

        @Override
        protected String getDatabaseName() {
            return "test";
        }

        @Override
        public MongoClient mongo() throws UnknownHostException {
            return new Fongo("Fake Mongo").getMongo();
        }
    }

    @Autowired
    MongoClient mongo;

    @Autowired
    OntologyConfigRepo repository;

    private OntologyConfig saved;

    @Before
    public void setup() {
        List<OntologyMapping> mappings = Lists.newArrayList(
                new OntologyMapping(IRI.create("http://example.com"), IRI.create("http://example.com/location")),
                new OntologyMapping(IRI.create("http://example2.com"), IRI.create("http://example2.com/location"))
        );
        saved = new OntologyConfig(mappings);

        mongo.dropDatabase("test");
    }

    @Test
    public void roundtrip() {

        repository.save(saved);

        OntologyConfig loaded = repository.findByHash(saved.getHash());
        assertEquals(saved, loaded);
    }

    @Test
    public void saveOnlyIfNotExists() {

        repository.save(saved);
        repository.save(saved);
        repository.save(saved);
        repository.save(saved);
        repository.save(saved);
        repository.save(saved);

        assertThat(count(saved.getHash()), equalTo(1L));
    }

    private long count(String hash) {
        MongoDatabase db = mongo.getDatabase("test");
        MongoCollection<Document> collection = db.getCollection("ontologyConfig");
        Bson query = new BsonDocument("hash", new BsonString(hash));
        return collection.count(query);
    }
}

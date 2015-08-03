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
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.dao.DuplicateKeyException;
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

    private List<OntologyMapping> mappings;

    @Before
    public void setup() {
        mappings = Lists.newArrayList(
                new OntologyMapping(IRI.create("http://example.com"), IRI.create("http://example.com/location")),
                new OntologyMapping(IRI.create("http://example2.com"), IRI.create("http://example2.com/location"))
        );
    }

    @After
    public void tearDown() {
        clear();
    }

    @Test
    public void roundtrip() {

        OntologyConfig saved = new OntologyConfig(mappings);

        repository.save(saved);

        OntologyConfig loaded = repository.findByHash(saved.getHash());
        assertEquals(saved, loaded);
    }

    @Test(expected = DuplicateKeyException.class)
    public void cannotSaveDuplicates() {

        repository.save(new OntologyConfig(mappings));
        repository.save(new OntologyConfig(mappings));
    }

    private long count(String hash) {
        Bson query = new BsonDocument("hash", new BsonString(hash));
        return getCollection().count(query);
    }

    private void clear() {
        Bson query = new BsonDocument();
        getCollection().deleteMany(query);
    }

    private MongoCollection<Document> getCollection() {
        MongoDatabase db = mongo.getDatabase("test");
        return db.getCollection("ontologyConfig");
    }
}

package org.coode.www.repository;

import com.github.fakemongo.Fongo;
import com.google.common.collect.Lists;
import com.mongodb.*;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.BSON;
import org.bson.BsonDocument;
import org.bson.BsonString;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.OntologyMapping;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.config.AbstractMongoConfiguration;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.index.Index;
import org.springframework.data.mongodb.core.query.Order;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
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
public class OntologyConfigRepoTest {

    @Configuration
    @EnableMongoRepositories
    static class ContextConfiguration extends AbstractMongoConfiguration {

        @Override
        protected String getDatabaseName() {
            return "test";
        }

        @Override
        @Bean
        public MongoClient mongo() throws UnknownHostException {
            return new Fongo("Fake Mongo").getMongo();
//          return new MongoClient("localhost", 27017); // use real Mongo for integration tests
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

    @Autowired
    MongoClient mongo;

    @Autowired
    OntologyConfigRepo repository;

    @Before
    public void setup() {
        mongo.dropDatabase("test");
    }

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

    @Test
    public void saveOnlyIfNotExists() {
        List<OntologyMapping> mappings = Lists.newArrayList(
                new OntologyMapping(IRI.create("http://example.com"), IRI.create("http://example.com/location")),
                new OntologyMapping(IRI.create("http://example2.com"), IRI.create("http://example2.com/location"))
        );
        OntologyConfig saved = new OntologyConfig(mappings);

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

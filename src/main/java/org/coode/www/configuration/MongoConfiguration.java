package org.coode.www.configuration;

import com.google.common.collect.Lists;
import com.mongodb.MongoClient;
import org.coode.www.model.OntologyConfig;
import org.coode.www.repository.IRIReadConverter;
import org.coode.www.repository.IRIWriteConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.config.AbstractMongoConfiguration;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.WriteResultChecking;
import org.springframework.data.mongodb.core.convert.CustomConversions;
import org.springframework.data.mongodb.core.convert.MappingMongoConverter;
import org.springframework.data.mongodb.core.index.Index;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

import java.net.UnknownHostException;
import java.util.List;

@Configuration
@EnableMongoRepositories
public class MongoConfiguration extends AbstractMongoConfiguration {

    @Value("#{systemProperties['mongo.host'] ?: 'localhost'}")
    protected String mongoHost;

    @Value("#{systemProperties['mongo.port'] ?: 27017}")
    protected int mongoPort;

    @Override
    protected String getDatabaseName() {
        return "ontology-browser";
    }

    @Override
    @Bean
    public MongoClient mongo() throws UnknownHostException {
        return new MongoClient(mongoHost, mongoPort);
    }

    @Override
    @Bean
    public CustomConversions customConversions() {
        List<Converter<?, ?>> converters = Lists.newArrayList();
        converters.add(new IRIReadConverter());
        converters.add(new IRIWriteConverter());
        return new CustomConversions(converters);
    }

    @Override
    @Bean
    public MappingMongoConverter mappingMongoConverter() throws Exception {
        MappingMongoConverter converter = super.mappingMongoConverter();
        converter.setCustomConversions(customConversions());
        return converter;
    }

    @Override
    @Bean
    public MongoTemplate mongoTemplate() throws Exception {
        MongoTemplate template = super.mongoTemplate();
        template.indexOps(OntologyConfig.class).ensureIndex(new Index().on("hash", Sort.Direction.ASC));
        template.setWriteResultChecking(WriteResultChecking.EXCEPTION);
        return template;
    }

    @Override
    protected String getMappingBasePackage() {
        return "org.coode.www.repository";
    }
}

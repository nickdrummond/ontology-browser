package org.coode.www.configuration;

import com.google.common.collect.Lists;
import com.mongodb.MongoClient;
import com.mongodb.MongoClientURI;
import org.coode.www.repository.IRIReadConverter;
import org.coode.www.repository.IRIWriteConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.mongodb.config.AbstractMongoConfiguration;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.WriteResultChecking;
import org.springframework.data.mongodb.core.convert.CustomConversions;
import org.springframework.data.mongodb.core.convert.MappingMongoConverter;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

import java.net.UnknownHostException;
import java.util.List;

@Configuration
@EnableMongoRepositories({"org.coode.www.repository"})
public class MongoConfiguration extends AbstractMongoConfiguration {

    @Value("#{environment.MONGOLAB_URI ?: 'mongodb://localhost:27017/ontology-browser'}")
    protected String mongoUri;

    @Override
    protected String getDatabaseName() {
        int dbNameIndex = mongoUri.lastIndexOf("/");
        return mongoUri.substring(dbNameIndex+1);
    }

    @Override
    @Bean
    public MongoClient mongo() throws UnknownHostException {
        MongoClientURI uri = new MongoClientURI(mongoUri);
        return new MongoClient(uri);
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
        template.setWriteResultChecking(WriteResultChecking.EXCEPTION);
        return template;
    }

    @Override
    protected String getMappingBasePackage() {
        return "org.coode.www.repository";
    }
}

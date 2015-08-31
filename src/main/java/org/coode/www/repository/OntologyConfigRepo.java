package org.coode.www.repository;

import org.coode.www.model.OntologyConfig;
import org.springframework.data.mongodb.repository.MongoRepository;

// See http://spring.io/guides/gs/accessing-data-mongodb/
public interface OntologyConfigRepo extends MongoRepository<OntologyConfig, String>{

    OntologyConfig findByHash(String hash);
}

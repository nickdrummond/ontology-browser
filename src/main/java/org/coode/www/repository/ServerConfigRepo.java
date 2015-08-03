package org.coode.www.repository;

import org.coode.www.model.ServerConfig;
import org.springframework.data.mongodb.repository.MongoRepository;

// See http://spring.io/guides/gs/accessing-data-mongodb/
public interface ServerConfigRepo extends MongoRepository<ServerConfig, String>{

    ServerConfig findByHash(String hash);
}

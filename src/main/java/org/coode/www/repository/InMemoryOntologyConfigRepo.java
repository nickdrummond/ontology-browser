package org.coode.www.repository;

import com.google.common.collect.Maps;
import org.coode.www.model.OntologyConfig;
import org.springframework.stereotype.Repository;

import java.util.Map;

@Repository
public class InMemoryOntologyConfigRepo {

    private Map<String, OntologyConfig> values = Maps.newHashMap();

    public OntologyConfig findByHash(String hash) {
        return values.get(hash);
    }

    public OntologyConfig save(OntologyConfig config) {
        values.put(config.getHash(), config);
        return config;
    }
}

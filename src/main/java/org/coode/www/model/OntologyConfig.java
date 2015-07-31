package org.coode.www.model;

import org.coode.www.util.Hashing;
import org.springframework.data.annotation.Id;

import javax.annotation.Nonnull;
import java.util.List;

public class OntologyConfig {

    @Id
    private String id;

    private String hash;

    private List<OntologyMapping> mappings;

    public OntologyConfig() {
    }

    public OntologyConfig(@Nonnull List<OntologyMapping> mappings) {
        this.mappings = mappings;
        this.hash = generateHash(mappings);
    }

    public String getHash() {
        return hash;
    }

    public void setHash(String hash) {
        this.hash = hash;
    }

    public List<OntologyMapping> getMappings() {
        return mappings;
    }

    public void setMappings(List<OntologyMapping> mappings) {
        this.mappings = mappings;
    }

    private String generateHash(List<OntologyMapping> mappings) {
        StringBuilder sb = new StringBuilder();
        for (OntologyMapping mapping : mappings) {
            sb.append(mapping.getOntologyIRI());
            sb.append(mapping.getLocationIRI());
        }
        return Hashing.md5(sb.toString());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        OntologyConfig that = (OntologyConfig) o;

        if (!hash.equals(that.hash)) return false;
        return mappings.equals(that.mappings);
    }

    @Override
    public int hashCode() {
        return hash.hashCode();
    }

    @Override
    public String toString() {
        return "OntologyConfig{" +
                "id='" + id + '\'' +
                ", hash='" + hash + '\'' +
                ", mappings=" + mappings +
                '}';
    }
}

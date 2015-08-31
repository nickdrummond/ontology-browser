package org.coode.www.model;

import com.google.common.base.Optional;
import com.google.common.collect.Lists;
import org.coode.www.util.Hashing;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

@Document
public class OntologyConfig {

    @Id
    private String id;

    @Indexed(unique = true)
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

        return hash.equals(that.hash) && mappings.equals(that.mappings);
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

    public Optional<IRI> documentFor(IRI ontologyIRI) {
        for (OntologyMapping mapping : mappings) {
            if (mapping.getOntologyIRI().equals(ontologyIRI)) {
                return Optional.of(mapping.getLocationIRI());
            }
        }
        return Optional.absent();
    }

    /**
     * Factory method for generating an OntologyConfig from an ontology and its imports closure.
     * @param activeOnt The active ontology.
     * @param ontologies The
     * @return an OntologyConfig representing the loaded ontologies.
     */
    public static OntologyConfig ontConfigFor(final OWLOntology activeOnt, Set<OWLOntology> ontologies) {
        List<OntologyMapping> mappings = Lists.newArrayList();
        mappings.add(mappingFor(activeOnt));
        Stream<OWLOntology> allOnts = ontologies.stream();
        allOnts.filter(ont -> !ont.equals(activeOnt)).forEach(ont -> mappings.add(mappingFor(ont)));
        return new OntologyConfig(mappings);
    }

    private static OntologyMapping mappingFor(final OWLOntology ont) {
        IRI docIRI = ont.getOWLOntologyManager().getOntologyDocumentIRI(ont);
        IRI ontIRI = ont.getOntologyID().getDefaultDocumentIRI().or(docIRI);
        return new OntologyMapping(ontIRI, docIRI);
    }
}

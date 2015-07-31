package org.coode.www.model;

import org.semanticweb.owlapi.model.IRI;

import javax.annotation.Nonnull;

public class OntologyMapping {

    private String ontologyIRI;
    private String locationIRI;

    public OntologyMapping() {
    }

    public OntologyMapping(@Nonnull IRI ontologyIRI, @Nonnull IRI locationIRI) {
        this.ontologyIRI = ontologyIRI.toQuotedString();
        this.locationIRI = locationIRI.toQuotedString();
    }

    public String getOntologyIRI() {
        return ontologyIRI;
    }

    public void setOntologyIRI(String ontologyIRI) {
        this.ontologyIRI = ontologyIRI;
    }

    public String getLocationIRI() {
        return locationIRI;
    }

    public void setLocationIRI(String locationIRI) {
        this.locationIRI = locationIRI;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        OntologyMapping that = (OntologyMapping) o;

        if (!ontologyIRI.equals(that.ontologyIRI)) return false;
        return locationIRI.equals(that.locationIRI);

    }

    @Override
    public int hashCode() {
        int result = ontologyIRI.hashCode();
        result = 31 * result + locationIRI.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return "OntologyMapping{" +
                "ontologyIRI='" + ontologyIRI + '\'' +
                ", locationIRI='" + locationIRI + '\'' +
                '}';
    }
}

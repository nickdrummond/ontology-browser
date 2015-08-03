package org.coode.www.model;

import org.coode.www.util.Hashing;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import javax.annotation.Nonnull;

@Document
public class ServerConfig {

    @Id
    private String id;

    @Indexed(unique = true)
    private String hash;

    private String renderer = "label";
    private IRI labelAnnotationIri = IRI.create("http://www.w3.org/2000/01/rdf-schema#label");
    private IRI labelPropertyUri = IRI.create("http://xmlns.com/foaf/0.1/name");
    private String labelLang = "";
    private String reasoner = "Structural Reasoner";

    public ServerConfig() {
        refreshHash();
    }

    public ServerConfig(@Nonnull final String renderer,
                        @Nonnull final IRI labelAnnotationIri,
                        @Nonnull final IRI labelPropertyUri,
                        @Nonnull final String labelLang,
                        @Nonnull final String reasoner) {
        this.renderer = renderer;
        this.labelAnnotationIri = labelAnnotationIri;
        this.labelPropertyUri = labelPropertyUri;
        this.labelLang = labelLang;
        this.reasoner = reasoner;
        refreshHash();
    }

    private void refreshHash() {
        hash = Hashing.md5(
                renderer + "|" +
                labelAnnotationIri + "|" +
                labelPropertyUri + "|" +
                labelLang + "|" +
                reasoner
        );
    }

    public String getHash() {
        return hash;
    }

    public void setHash(String hash) {
        this.hash = hash;
    }

    public String getRenderer() {
        return renderer;
    }

    public void setRenderer(String renderer) {
        this.renderer = renderer;
    }

    public IRI getLabelAnnotationIri() {
        return labelAnnotationIri;
    }

    public void setLabelAnnotationIri(IRI labelAnnotationIri) {
        this.labelAnnotationIri = labelAnnotationIri;
    }

    public IRI getLabelPropertyUri() {
        return labelPropertyUri;
    }

    public void setLabelPropertyUri(IRI labelPropertyUri) {
        this.labelPropertyUri = labelPropertyUri;
    }

    public String getLabelLang() {
        return labelLang;
    }

    public void setLabelLang(String labelLang) {
        this.labelLang = labelLang;
    }

    public String getReasoner() {
        return reasoner;
    }

    public void setReasoner(String reasoner) {
        this.reasoner = reasoner;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ServerConfig that = (ServerConfig) o;

        if (!renderer.equals(that.renderer)) return false;
        if (!labelAnnotationIri.equals(that.labelAnnotationIri)) return false;
        if (!labelPropertyUri.equals(that.labelPropertyUri)) return false;
        if (!labelLang.equals(that.labelLang)) return false;
        return reasoner.equals(that.reasoner);

    }

    @Override
    public int hashCode() {
        int result = renderer.hashCode();
        result = 31 * result + labelAnnotationIri.hashCode();
        result = 31 * result + labelPropertyUri.hashCode();
        result = 31 * result + labelLang.hashCode();
        result = 31 * result + reasoner.hashCode();
        return result;
    }
}

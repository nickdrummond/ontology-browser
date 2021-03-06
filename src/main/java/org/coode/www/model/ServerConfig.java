package org.coode.www.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.coode.www.util.Hashing;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public class ServerConfig {

    @Id
    private String id;

    @Indexed(unique = true)
    private String hash;

    @JsonProperty
    private String renderer = "label";
    @JsonProperty
    private IRI labelAnnotationIri = IRI.create("http://www.w3.org/2000/01/rdf-schema#label");
    @JsonProperty
    private IRI labelPropertyIri = IRI.create("http://xmlns.com/foaf/0.1/name");
    @JsonProperty
    private String labelLang = "";
    @JsonProperty
    private String reasoner = "Structural Reasoner";

    public ServerConfig() {
        refreshHash();
    }

    public String getHash() {
        return hash;
    }

    public String getRenderer() {
        return renderer;
    }

    public IRI getLabelAnnotationIri() {
        return labelAnnotationIri;
    }

    public IRI getLabelPropertyIri() {
        return labelPropertyIri;
    }

    public String getLabelLang() {
        return labelLang;
    }

    public String getReasoner() {
        return reasoner;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ServerConfig that = (ServerConfig) o;

        if (!renderer.equals(that.renderer)) return false;
        if (!labelAnnotationIri.equals(that.labelAnnotationIri)) return false;
        if (!labelPropertyIri.equals(that.labelPropertyIri)) return false;
        if (!labelLang.equals(that.labelLang)) return false;
        return reasoner.equals(that.reasoner);

    }

    @Override
    public int hashCode() {
        int result = renderer.hashCode();
        result = 31 * result + labelAnnotationIri.hashCode();
        result = 31 * result + labelPropertyIri.hashCode();
        result = 31 * result + labelLang.hashCode();
        result = 31 * result + reasoner.hashCode();
        return result;
    }

    private void refreshHash() {
        hash = Hashing.md5(
                        renderer + "|" +
                        labelAnnotationIri + "|" +
                                labelPropertyIri + "|" +
                        labelLang + "|" +
                        reasoner
        );
    }

    @JsonIgnore
    public ServerConfig setOption(OptionSet optionSet) {
        ServerConfig copy = this.copy();
        switch(optionSet.getProperty()) {
            case "renderer": copy.renderer = optionSet.getValue(); break;
            case "labelAnnotationIri": copy.labelAnnotationIri = IRI.create(optionSet.getValue()); break;
            case "labelPropertyIri": copy.labelPropertyIri = IRI.create(optionSet.getValue()); break;
            case "labelLang": copy.labelLang = optionSet.getValue(); break;
            case "reasoner": copy.reasoner = optionSet.getValue(); break;
            default: return this;
        }
        copy.refreshHash();
        return copy;
    }

    private ServerConfig copy() {
        ServerConfig copy = new ServerConfig();
        copy.renderer = this.renderer;
        copy.labelAnnotationIri = this.labelAnnotationIri;
        copy.labelPropertyIri = this.labelPropertyIri;
        copy.labelLang = this.labelLang;
        copy.reasoner = this.reasoner;
        copy.refreshHash();
        return copy;
    }
}

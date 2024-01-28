package org.coode.www.service;

import com.google.common.collect.Streams;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.renderer.LabelShortFormProvider;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.model.providers.EntityProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.apache.lucene.document.Field.Store.YES;

@Service
public class LuceneSearchService implements SearchService {

    private static final Logger log = LoggerFactory.getLogger(LuceneSearchService.class);
    private final Directory directory;
    private final OWLHTMLKit kit;
    private final Analyzer analyzer;

    public LuceneSearchService(
            @Autowired Directory directory,
            @Autowired OWLHTMLKit kit) throws IOException {
        this.directory = directory;
        this.kit = kit;
        IRI labelProp = OWLRDFVocabulary.RDFS_LABEL.getIRI();
        OWLAnnotationProperty labelAnnotationProperty = kit.getOWLOntologyManager().getOWLDataFactory().getOWLAnnotationProperty(labelProp);

        LabelShortFormProvider labelSfp = new LabelShortFormProvider(labelAnnotationProperty, "", kit.getOntologies(), kit.getShortFormProvider());

        log.info("Building lucene search index");
        // TODO sort out dependencies
        analyzer = new StandardAnalyzer();
        IndexWriterConfig conf = new IndexWriterConfig(analyzer);
        IndexWriter writer = new IndexWriter(directory, conf);

        OWLOntology ont = kit.getActiveOntology();

        Streams.concat(
                        ont.classesInSignature(Imports.INCLUDED),
                        ont.individualsInSignature(Imports.INCLUDED),
                        ont.objectPropertiesInSignature(Imports.INCLUDED),
                        ont.dataPropertiesInSignature(Imports.INCLUDED),
                        ont.annotationPropertiesInSignature(Imports.INCLUDED),
                        ont.datatypesInSignature(Imports.INCLUDED)
                )
                .forEach(i -> {
                    try {
                        writer.addDocument(createDocumentFromEntity(i, labelSfp));
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                });
        writer.close();
        log.info("Completed lucene search index");
    }

    private Document createDocumentFromEntity(OWLEntity entity, LabelShortFormProvider sfp) {
        Document doc = new Document();
        doc.add(new StringField("iri", entity.getIRI().getIRIString(), YES));
        doc.add(new StringField("type", entity.getEntityType().getName(), YES));
        doc.add(new TextField("shortform", kit.getShortFormProvider().getShortForm(entity), YES));
        doc.add(new TextField("label", sfp.getShortForm(entity), YES));
        return doc;
    }

    @Override
    public List<OWLEntity> findByName(String input, OWLHTMLKit kit) {
        try {
            IndexReader reader = DirectoryReader.open(directory);

            IndexSearcher searcher = new IndexSearcher(reader);

            Query q = new DisjunctionMaxQuery(List.of(
                    new PrefixQuery(new Term("label", input)),
                    new FuzzyQuery(new Term("label", input))), 0.1f);

            log.info(q.toString());

            TopDocs results = searcher.search(q, 20);

            OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

            return Arrays.stream(results.scoreDocs).map(hit -> {
                try {
                    Document document = reader.storedFields().document(hit.doc);
                    IRI iri = IRI.create(document.get("iri"));
                    return getEntityFromString(document.get("type"), iri, df);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }).toList();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private OWLEntity getEntityFromString(String type, IRI iri, EntityProvider df) {
        return EntityType.values().stream()
                .filter(t -> t.getName().equals(type))
                .findFirst().orElseThrow(() -> new RuntimeException("Unknown ")).buildEntity(iri, df);
    }

    @Override
    public List<AxiomWithMetadata> findByAnnotation(@Nonnull String value, OWLAnnotationProperty searchProp, @Nonnull OWLHTMLKit kit) {
        return null;
    }

    @Override
    public Set<OWLEntity> getEntities(IRI iri, OWLHTMLKit kit) {
        return null;
    }
}

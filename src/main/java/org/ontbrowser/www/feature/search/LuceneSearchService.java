package org.ontbrowser.www.feature.search;

import com.google.common.collect.Streams;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.DelegatingAnalyzerWrapper;
import org.apache.lucene.analysis.core.KeywordAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.renderer.LabelShortFormProvider;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.model.providers.EntityProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import static org.apache.lucene.analysis.Analyzer.PER_FIELD_REUSE_STRATEGY;
import static org.apache.lucene.document.Field.Store.YES;

@Profile("lucene")
@Service
public class LuceneSearchService implements SearchService {

    private static final Logger log = LoggerFactory.getLogger(LuceneSearchService.class);
    public static final String SHORTFORM = "shortform";
    public static final String LABEL = "label";
    public static final String IRI = "iri";
    public static final String TYPE = "type";
    private final Directory directory;
    private final OWLHTMLKit kit;
    private final Analyzer analyzer;

    public LuceneSearchService(
            @Autowired Directory directory,
            @Autowired OWLHTMLKit kit) throws IOException {
        this.directory = directory;
        this.kit = kit;
        var df = kit.getOWLOntologyManager().getOWLDataFactory();

        var labelProp = OWLRDFVocabulary.RDFS_LABEL.getIRI();
        var labelAnnotationProperty = df.getOWLAnnotationProperty(labelProp);
        var labelSfp = new LabelShortFormProvider(
                labelAnnotationProperty,
                "",
                kit.getOntologies(),
                kit.getShortFormProvider());

        log.info("Building lucene search index");

        this.analyzer = new DelegatingAnalyzerWrapper(PER_FIELD_REUSE_STRATEGY) {
            private final Analyzer defaultAnalyzer = new StandardAnalyzer();
            private final Analyzer keywordAnalyzer = new KeywordAnalyzer();

            @Override
            protected Analyzer getWrappedAnalyzer(String fieldName) {
                return switch (fieldName) {
                    case IRI, TYPE, SHORTFORM -> keywordAnalyzer;
                    default -> defaultAnalyzer;
                };
            }
        };

        IndexWriterConfig conf = new IndexWriterConfig(analyzer);
        IndexWriter writer = new IndexWriter(directory, conf);

        OWLOntology ont = kit.getRootOntology();

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
        doc.add(new StringField(IRI, entity.getIRI().getIRIString(), YES));
        doc.add(new StringField(TYPE, entity.getEntityType().getName(), YES));
        doc.add(new TextField(SHORTFORM, kit.getShortFormProvider().getShortForm(entity), YES));
        doc.add(new TextField(LABEL, sfp.getShortForm(entity), YES));
        return doc;
    }

    @Override
    public List<OWLEntity> findByName(String input, OWLHTMLKit kit) {
        try {
            IndexReader reader = DirectoryReader.open(directory);

            IndexSearcher searcher = new IndexSearcher(reader);

            // whole shortform
            var wholeThing = new PrefixQuery(new Term(SHORTFORM, input));

            var words = input.split(" ");

            // or fuzzy match any word
            var builder = new BooleanQuery.Builder();
            for (String word : words) {
                var fuzzyQuery = new FuzzyQuery(new Term(LABEL, word), 2);
                builder.add(fuzzyQuery, BooleanClause.Occur.SHOULD);
            }
            Query any = builder.build();

            Query q = new DisjunctionMaxQuery(List.of(wholeThing, any), 0.5f);

            log.debug("Lucene search {}", q);

            TopDocs results = searcher.search(q, 20, Sort.RELEVANCE);

            OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

            return Arrays.stream(results.scoreDocs).map(hit -> {
                try {
                    Document document = reader.storedFields().document(hit.doc);
                    IRI iri = org.semanticweb.owlapi.model.IRI.create(document.get(IRI));
                    return getEntityFromString(document.get(TYPE), iri, df);
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
        throw new NotImplementedException("Find by annotation not implemented");
    }
}
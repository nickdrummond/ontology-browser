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
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.RestartListener;
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
public class LuceneSearchService implements SearchService, RestartListener {

    private static final Logger log = LoggerFactory.getLogger(LuceneSearchService.class);

    public static final String SHORTFORM_FIELD = "shortform";
    public static final String LABEL_FIELD = "label";
    public static final String IRI_FIELD = "iri";
    public static final String TYPE_FIELD = "type";

    private final Directory directory;
    private final OWLHTMLKit kit;
    private final Analyzer analyzer;

    public LuceneSearchService(
            @Autowired Directory directory,
            @Autowired OWLHTMLKit kit) {
        this.directory = directory;
        this.kit = kit;
        this.analyzer = new DelegatingAnalyzerWrapper(PER_FIELD_REUSE_STRATEGY) {
            private final Analyzer defaultAnalyzer = new StandardAnalyzer();
            private final Analyzer keywordAnalyzer = new KeywordAnalyzer();

            @Override
            protected Analyzer getWrappedAnalyzer(String fieldName) {
                return switch (fieldName) {
                    case IRI_FIELD, TYPE_FIELD, SHORTFORM_FIELD -> keywordAnalyzer;
                    default -> defaultAnalyzer;
                };
            }
        };
        doIndex();
    }

    @Override
    public void onRestart() {
        doIndex();
    }

    private void doIndex() {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();

        var labelProp = OWLRDFVocabulary.RDFS_LABEL.getIRI();
        var labelAnnotationProperty = df.getOWLAnnotationProperty(labelProp);
        var labelSfp = new LabelShortFormProvider(
                labelAnnotationProperty,
                "",
                kit.getOntologies(),
                kit.getShortFormProvider());

        log.info("Building lucene search index");

        var conf = new IndexWriterConfig(analyzer);
        conf.setOpenMode(IndexWriterConfig.OpenMode.CREATE); // overwrite index

        try (var writer = new IndexWriter(directory, conf)) {
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
            writer.commit();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        log.info("Completed lucene search index");
    }

    private Document createDocumentFromEntity(OWLEntity entity, LabelShortFormProvider sfp) {
        Document doc = new Document();
        doc.add(new StringField(IRI_FIELD, entity.getIRI().getIRIString(), YES));
        doc.add(new StringField(TYPE_FIELD, entity.getEntityType().getName(), YES));
        doc.add(new TextField(SHORTFORM_FIELD, kit.getShortFormProvider().getShortForm(entity), YES));
        doc.add(new TextField(LABEL_FIELD, sfp.getShortForm(entity), YES));
        return doc;
    }

    @Override
    public List<OWLEntity> findByName(String input, OWLHTMLKit kit) {
        try (var reader = DirectoryReader.open(directory)) {

            var searcher = new IndexSearcher(reader);

            var q = buildQuery(input);

            log.debug("Lucene search {}", q);

            var results = searcher.search(q, 20, Sort.RELEVANCE);

            return toOwlEntities(results, reader);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private List<OWLEntity> toOwlEntities(TopFieldDocs results, DirectoryReader reader) {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();

        return Arrays.stream(results.scoreDocs)
                .map(hit -> {
                    try {
                        var document = reader.storedFields().document(hit.doc);
                        var iri = IRI.create(document.get(IRI_FIELD));
                        var type = document.get(TYPE_FIELD);
                        return getEntityFromString(type, iri, df);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                })
                .toList();
    }

    private Query buildQuery(String input) {

        // whole shortform
        var wholeThing = new PrefixQuery(new Term(SHORTFORM_FIELD, input));

        var words = input.split(" ");

        // or fuzzy match any word
        var builder = new BooleanQuery.Builder();
        for (String word : words) {
            var wordQuery = new FuzzyQuery(new Term(LABEL_FIELD, word), 2);
            builder.add(wordQuery, BooleanClause.Occur.SHOULD);
        }
        var any = builder.build();

        return new DisjunctionMaxQuery(List.of(wholeThing, any), 0.5f);
    }

    private OWLEntity getEntityFromString(String type, IRI iri, EntityProvider df) {
        return EntityType.values().stream()
                .filter(t -> t.getName().equals(type))
                .findFirst()
                .orElseThrow(() -> new RuntimeException("Unknown "))
                .buildEntity(iri, df);
    }

    @Override
    public List<AxiomWithMetadata> findByAnnotation(@Nonnull String value, OWLAnnotationProperty searchProp, @Nonnull OWLHTMLKit kit) {
        throw new NotImplementedException("Find by annotation not implemented");
    }

}
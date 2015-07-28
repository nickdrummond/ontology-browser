package org.coode.www.renderer;

import com.google.common.base.Function;
import com.google.common.base.Optional;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntax;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLFacet;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;

public class OWLHTMLVisitor implements OWLObjectVisitor {

    // These should match the css class names
    private static final String CSS_ACTIVE_ENTITY = "active-entity";
    private static final String CSS_KEYWORD = "keyword";
    private static final String CSS_ONTOLOGY_URI = "ontology-uri";
    private static final String CSS_ACTIVE_ONTOLOGY_URI = "active-ontology-uri";
    private static final String CSS_SOME = "some";
    private static final String CSS_ONLY = "only";
    private static final String CSS_VALUE = "value";
    private static final String CSS_LITERAL = "literal";

    public static final String EQUIV_CHAR = "&equiv;";
    public static final String SUBCLASS_CHAR = "&sube;";

    // the subset and equivalence symbols can be encoded in HTML
    private final boolean USE_SYMBOLS = true;

    // the object currently on the page (will be highlighted)
    private final Optional<? extends OWLObject> activeObject;

    private final URLScheme urlScheme;

    private final ShortFormProvider sfProvider;

    private final OntologyIRIShortFormProvider ontologyIriSFProvider;

    private final Set<OWLOntology> ontologies;

    private final OWLOntology activeOntology;

    private PrintWriter out;

    private int indent = 0;

    public OWLHTMLVisitor(
            ShortFormProvider sfProvider,
            OntologyIRIShortFormProvider ontologySFProvider,
            URLScheme urlScheme,
            Set<OWLOntology> ontologies,
            OWLOntology activeOntology,
            Optional<? extends OWLObject> activeObject) {

        this.sfProvider = sfProvider;
        this.ontologyIriSFProvider = ontologySFProvider;
        this.urlScheme = urlScheme;
        this.ontologies = ImmutableSet.copyOf(ontologies);
        this.activeOntology = activeOntology;
        this.activeObject = activeObject;
    }

    public void setWriter(PrintWriter out) {
        this.out = out;
    }

    private void write(String s) {
        out.write(s);
    }

    ////////// Ontology

    public void visit(@Nonnull OWLOntology ontology) {
        final URL urlForOntology = urlScheme.getURLForOWLObject(ontology);
        String link = urlForOntology.toString();
        String cssClass = CSS_ONTOLOGY_URI;
        if (activeOntology != null && ontology.equals(activeOntology)){
            cssClass = CSS_ACTIVE_ONTOLOGY_URI;
        }

        boolean writeLink = false;

        if (!activeObject.isPresent()){
            writeLink = true;
        }
        else{
            if (!activeObject.get().equals(ontology)){
                writeLink = true;
            }
            else{
                write("<span class='" + CSS_ACTIVE_ENTITY + " " + cssClass + "'>");
                write(ontologyIriSFProvider.getShortForm(ontology));
                write("</span>");
            }
        }

        if (writeLink){
            write("<a class='" + cssClass + "'");
            String id = getOntologyIdString(ontology);
            write(" href=\"" + link + "\" title='" + id + "'");
            write(">");
            write(ontologyIriSFProvider.getShortForm(ontology));
            write("</a>");
        }
    }

    private String getOntologyIdString(final OWLOntology ont){
        return ont.getOntologyID().getDefaultDocumentIRI().transform(new Function<IRI, String>(){
            @Nullable
            @Override
            public String apply(IRI iri) {
                return iri.toString();
            }
        }).or(ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString());
    }

    ////////// Entities

    public void visit(@Nonnull OWLClass desc) {
        writeOWLEntity(desc, NamedObjectType.classes.getSingularRendering());
    }

    public void visit(@Nonnull OWLDataProperty property) {
        writeOWLEntity(property, NamedObjectType.dataproperties.getSingularRendering());
    }

    public void visit(@Nonnull OWLObjectProperty property) {
        writeOWLEntity(property, NamedObjectType.objectproperties.getSingularRendering());
    }

    public void visit(@Nonnull OWLAnnotationProperty property) {
        writeOWLEntity(property, NamedObjectType.annotationproperties.getSingularRendering());
    }

    public void visit(@Nonnull OWLNamedIndividual individual) {
        writeOWLEntity(individual, NamedObjectType.individuals.getSingularRendering());
    }

    public void visit(@Nonnull OWLDatatype datatype) {
        writeOWLEntity(datatype, NamedObjectType.datatypes.getSingularRendering());
    }

    public void visit(@Nonnull IRI iri) {
        writeIRIWithBoldFragment(iri, iri.getShortForm());
    }

    public void visit(@Nonnull OWLAnonymousIndividual individual) {
        writeAnonymousIndividual(individual);
    }

    ///////// Anonymous classes

    public void visit(@Nonnull OWLObjectSomeValuesFrom desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.SOME.toString(), CSS_SOME);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLObjectAllValuesFrom desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.ONLY.toString(), CSS_ONLY);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLObjectHasValue desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.VALUE.toString(), CSS_VALUE);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLObjectMinCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MIN.toString());
    }

    public void visit(@Nonnull OWLObjectExactCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.EXACTLY.toString());
    }

    public void visit(@Nonnull OWLObjectMaxCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MAX.toString());
    }

    public void visit(@Nonnull OWLObjectComplementOf desc) {
        writeKeyword(ManchesterOWLSyntax.NOT.toString());
        write(" ");
        writeOp(desc.getOperand(), false);
    }

    public void visit(@Nonnull OWLObjectHasSelf desc) {
        writeKeyword(ManchesterOWLSyntax.SELF.toString());
    }

    public void visit(@Nonnull OWLObjectIntersectionOf desc) {
        writeKeywordOpList(orderOps(desc.getOperands()), ManchesterOWLSyntax.AND.toString(), true);
    }

    public void visit(@Nonnull OWLObjectUnionOf desc) {
        writeKeywordOpList(orderOps(desc.getOperands()), ManchesterOWLSyntax.OR.toString(), false);
    }

    public void visit(@Nonnull OWLObjectOneOf desc) {
        write("{");
        writeOpList(desc.getIndividuals(), ", ", false);
        write("}");
    }

    public void visit(@Nonnull OWLDataOneOf desc) {
        write("{");
        writeOpList(desc.getValues(), ", ", false);
        write("}");
    }

    public void visit(@Nonnull OWLDataSomeValuesFrom desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.SOME.toString(), CSS_SOME);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLDataAllValuesFrom desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.ONLY.toString(), CSS_ONLY);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLDataHasValue desc) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.VALUE.toString(), CSS_VALUE);
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    public void visit(@Nonnull OWLDataMinCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MIN.toString());
    }

    public void visit(@Nonnull OWLDataExactCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.EXACTLY.toString());
    }

    public void visit(@Nonnull OWLDataMaxCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MAX.toString());
    }

    public void visit(@Nonnull OWLDatatypeRestriction node) {
        node.getDatatype().accept(this);
        write(" [");
        writeOpList(node.getFacetRestrictions(), ", ", false);
        write("]");
    }

    public void visit(@Nonnull OWLFacetRestriction node) {
        writeKeyword(writeFacet(node.getFacet()));
        node.getFacetValue().accept(this);
    }

    public void visit(@Nonnull OWLDataComplementOf node) {
        writeKeyword(ManchesterOWLSyntax.NOT.toString());
        write(" ");
        writeOp(node.getDataRange(), true);
    }

    public void visit(@Nonnull OWLDataIntersectionOf owlDataIntersectionOf) {
        writeKeywordOpList(owlDataIntersectionOf.getOperands(), ManchesterOWLSyntax.AND.toString(), true);
    }

    public void visit(@Nonnull OWLDataUnionOf owlDataUnionOf) {
        writeKeywordOpList(owlDataUnionOf.getOperands(), ManchesterOWLSyntax.OR.toString(), false);
    }

    ////////// Properties

    public void visit(@Nonnull OWLObjectInverseOf property) {
        writeKeyword(ManchesterOWLSyntax.INVERSE_OF.toString());
        write(" ");
        writeOp(property.getInverse(), true);
    }

    public void visit(@Nonnull OWLFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.FUNCTIONAL.toString());
    }

    public void visit(@Nonnull OWLInverseFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.INVERSE_FUNCTIONAL.toString());
    }

    public void visit(@Nonnull OWLSymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.SYMMETRIC.toString());
    }

    public void visit(@Nonnull OWLTransitiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.TRANSITIVE.toString());
    }

    public void visit(@Nonnull OWLAsymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.ASYMMETRIC.toString());
    }

    public void visit(@Nonnull OWLReflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.REFLEXIVE.toString());
    }

    public void visit(@Nonnull OWLIrreflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.IRREFLEXIVE.toString());
    }

    public void visit(@Nonnull OWLObjectPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.DOMAIN.toString());
        write(" ");
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLObjectPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(" ");
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLInverseObjectPropertiesAxiom axiom) {
        writeOp(axiom.getFirstProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.INVERSE.toString());
        write(" ");
        writeOp(axiom.getSecondProperty(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLHasKeyAxiom axiom) {
        writeOp(axiom.getClassExpression(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.HAS_KEY.toString());
        write(" ");
        write("(");
        writeOpList(axiom.getPropertyExpressions(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDatatypeDefinitionAxiom axiom) {
        axiom.getDatatype().accept(this);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.EQUIVALENT_TO.toString());
        write(" ");
        axiom.getDataRange().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull SWRLRule swrlRule) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLClassAtom swrlClassAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLDataRangeAtom swrlDataRangeAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLObjectPropertyAtom swrlObjectPropertyAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLDataPropertyAtom swrlDataPropertyAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLBuiltInAtom swrlBuiltInAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLVariable swrlVariable) {
        // @@TODO SWRL Support
    }

    public void visit(@Nonnull SWRLIndividualArgument swrlIndividualArgument) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLLiteralArgument swrlLiteralArgument) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLSameIndividualAtom swrlSameIndividualAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull SWRLDifferentIndividualsAtom swrlDifferentIndividualsAtom) {
        // @@TODO SWRL SUpport
    }

    public void visit(@Nonnull OWLSubPropertyChainOfAxiom axiom) {
        writeKeywordOpList(axiom.getPropertyChain(), "o", false);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(" ");
        writeOp(axiom.getSuperProperty(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDataPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.DOMAIN.toString());
        write(" ");
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDataPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(" ");
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLFunctionalDataPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.FUNCTIONAL.toString());
        writeAnnotations(axiom);
    }

    ////////// Annotations

    public void visit(@Nonnull OWLAnnotationAssertionAxiom axiom) {
        final OWLAnnotationSubject subject = axiom.getSubject();
        // extract the entities with this IRI
        if (subject instanceof IRI){
            Set<OWLEntity> entities = Sets.newHashSet();
            for (OWLOntology ont : ontologies){
                entities.addAll(ont.getEntitiesInSignature((IRI)subject));
            }
            if (!entities.isEmpty()){
                boolean started = false;
                for (OWLEntity entity : entities){
                    if (started){
                        write("&nbsp;");
                    }
                    entity.accept(this);
                    started = true;
                }
            }
            else{
                subject.accept(this);
            }
        }
        else{
            subject.accept(this);
        }
        write("&nbsp;");
        axiom.getAnnotation().accept(this);
        write("&nbsp;");
        writeAnnotations(axiom); // in theory, you could annotate the annotation axioms !!
    }

    public void visit(@Nonnull OWLSubAnnotationPropertyOfAxiom axiom) {
        writeOp(axiom.getSubProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(" ");
        writeOp(axiom.getSuperProperty(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLAnnotationPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(" ");
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLAnnotationPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(" ");
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(" ");
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLAnnotation annotation) {
        annotation.getProperty().accept(this);
        write(" ");
        annotation.getValue().accept(this);
    }

    // OWLAPI v3.1
    public void visit(@Nonnull OWLLiteral node) {
        write("<span class='" + CSS_LITERAL + "'>");
        final OWLDatatype dt = node.getDatatype();
        if (dt.isInteger() || dt.isFloat()){
            writeLiteralContents(node.getLiteral());
            write("</span>");
        }
        else{
            write("\"");
            writeLiteralContents(node.getLiteral());
            write("\"");
            write("</span>");
            if (node.isRDFPlainLiteral()){
                if (node.hasLang()){
                    final String lang = node.getLang();
                    write(" <span style='color: black;'>@" + lang + "</span>");
                }
            }
            else{
                write("(");
                dt.accept(this);
                write(")");
            }
        }
    }

    /////////// Axioms

    public void visit(@Nonnull OWLEquivalentClassesAxiom axiom) {
        writeEquivalence(orderOps(axiom.getClassExpressions()), axiom);
    }

    public void visit(@Nonnull OWLEquivalentObjectPropertiesAxiom axiom) {
        writeEquivalence(axiom.getProperties(), axiom);
    }

    public void visit(@Nonnull OWLEquivalentDataPropertiesAxiom axiom) {
        writeEquivalence(axiom.getProperties(), axiom);
    }

    public void visit(@Nonnull OWLSameIndividualAxiom axiom) {
        writeKeywordOpList(axiom.getIndividuals(), ManchesterOWLSyntax.SAME_AS.toString(), false);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLSubClassOfAxiom axiom) {
        axiom.getSubClass().accept(this);
        write(" ");
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUBCLASS_OF.toString());
        write(" ");
        axiom.getSuperClass().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLSubObjectPropertyOfAxiom axiom) {
        axiom.getSubProperty().accept(this);
        write(" ");
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(" ");
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLSubDataPropertyOfAxiom axiom) {
        axiom.getSubProperty().accept(this);
        write(" ");
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(" ");
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDisjointClassesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_CLASSES.toString());
        write("(");
        writeOpList(axiom.getClassExpressions(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDisjointObjectPropertiesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_PROPERTIES.toString());
        write("(");
        writeOpList(axiom.getProperties(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDisjointDataPropertiesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_PROPERTIES.toString());
        write("(");
        writeOpList(axiom.getProperties(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDifferentIndividualsAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DIFFERENT_INDIVIDUALS.toString());
        write("(");
        writeOpList(axiom.getIndividuals(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDisjointUnionAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_UNION_OF.toString());
        write("(");
        writeOpList(axiom.getClassExpressions(), ", ", false);
        write(")");
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLDeclarationAxiom axiom) {
        final OWLEntity entity = axiom.getEntity();
        if (entity.isOWLClass()){
            writeKeyword(ManchesterOWLSyntax.CLASS.toString());
            write(": ");
        }
        else if (entity.isOWLObjectProperty()){
            writeKeyword(ManchesterOWLSyntax.OBJECT_PROPERTY.toString());
            write(": ");
        }
        else if (entity.isOWLDataProperty()){
            writeKeyword(ManchesterOWLSyntax.DATA_PROPERTY.toString());
            write(": ");
        }
        else if (entity.isOWLAnnotationProperty()){
            writeKeyword(ManchesterOWLSyntax.ANNOTATION_PROPERTY.toString());
            write(": ");
        }
        else if (entity.isOWLNamedIndividual()){
            writeKeyword(ManchesterOWLSyntax.INDIVIDUAL.toString());
            write(": ");
        }
        else if (entity.isOWLDatatype()){
            writeKeyword("Datatype");
            write(": ");
        }
        entity.accept(this);
        writeAnnotations(axiom);
    }

    /////// OWLIndividual assertions

    public void visit(@Nonnull OWLClassAssertionAxiom axiom) {
        axiom.getIndividual().accept(this);
        write(": ");
        axiom.getClassExpression().accept(this);
        writeAnnotations(axiom);
    }

    public void visit(@Nonnull OWLObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(@Nonnull OWLDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(@Nonnull OWLNegativeObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    public void visit(@Nonnull OWLNegativeDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    private void writeEquivalence(Collection<? extends OWLObject> objects, OWLAxiom axiom) {
        String equiv = USE_SYMBOLS ? EQUIV_CHAR : ManchesterOWLSyntax.EQUIVALENT_TO.toString();
        writeKeywordOpList(objects, equiv, false);
        writeAnnotations(axiom);
    }

    private String getName(OWLEntity entity){
        return sfProvider.getShortForm(entity).replaceAll(" ", "&nbsp;");
    }

    // just make sure a named class is first if there is one
    private List<OWLClassExpression> orderOps(Set<OWLClassExpression> ops) {
        List<OWLClassExpression> orderedOps = new ArrayList<OWLClassExpression>(ops);
        Collections.sort(orderedOps, new Comparator<OWLClassExpression>() {
            public int compare(OWLClassExpression d1, OWLClassExpression d2) {
                if (d1 instanceof OWLClass) {
                    return -1;
                } else if (d2 instanceof OWLClass) {
                    return 1;
                }
                return 0;
            }
        });
        return orderedOps;
    }

    private void writeIRIWithBoldFragment(IRI iri, String shortForm) {
        // Encourage wrapping on / instead of other characters
        final String fullURI = iri.toString().replaceAll("/", "/<wbr>");

        int index = 0;
        if (shortForm != null) {
            index = fullURI.lastIndexOf(shortForm);
        }
        if (index == 0){
            write(fullURI);
        }
        else{
            write(fullURI.substring(0, index));
            write("<b>");
            write(shortForm);
            write("</b>");
            write(fullURI.substring(index+shortForm.length()));
        }
    }

    // add a span to allow for css highlighting
    private void writeKeyword(String keyword) {
        writeKeyword(keyword, CSS_KEYWORD);
    }

    // add a span to allow for css highlighting
    private void writeKeyword(String keyword, String cssClass) {
        write("<span class='" + cssClass + "'>" + keyword + "</span>");
    }

    // useful to add brackets around the anonymous operators of unions and intersections and the fillers of restrictions
    private void writeOp(OWLObject op, boolean wrap) {
        if (op instanceof OWLEntity ||
                op instanceof OWLObjectOneOf ||
                op instanceof OWLDataOneOf ||
                op instanceof OWLDatatypeRestriction ||
                op instanceof OWLLiteral){
            op.accept(this);
        }
        else{ // provide brackets for clarity
            write("(");
            if (wrap && op instanceof OWLObjectIntersectionOf){
                indent++;
                write("<br>");
                writeIndent();
            }
            op.accept(this);
            if (wrap && op instanceof OWLObjectIntersectionOf){
                indent--;
            }
            write(")");
        }
    }

    private void writeIndent() {
        for (int i=0; i<indent; i++){
            write("&nbsp;&nbsp;&nbsp;&nbsp;");
        }
    }

    private void writeOWLEntity(OWLEntity entity, String cssClass) {
        final URI uri = entity.getIRI().toURI();

        String name = getName(entity);

        Set<String> cssClasses = new HashSet<>();
        cssClasses.add(cssClass);

        if (!activeObject.isPresent()){
            final URL urlForTarget = urlScheme.getURLForOWLObject(entity);
            write("<a href=\"" + urlForTarget + "\"");
            writeCSSClasses(cssClasses);
            write(" title=\"" + uri + "\">" + name + "</a>");
        }
        else{
            if (activeObject.get().equals(entity)){
                cssClasses.add(CSS_ACTIVE_ENTITY);
                write("<span");
                writeCSSClasses(cssClasses);
                write(">" + name + "</span>");
            }
            else{
                final URL urlForTarget = urlScheme.getURLForOWLObject(entity);
                write("<a href=\"" + urlForTarget + "\"");

                writeCSSClasses(cssClasses);
                write(" title=\"" + uri + "\">");
                write(name);
                write("</a>");
            }
        }
    }

    private void writeAnonymousIndividual(OWLAnonymousIndividual individual) {
        final Collection<OWLClassExpression> types = EntitySearcher.getTypes(individual, ontologies);
        if (!types.isEmpty()){
            writeOpList(types, ", ", false);
        }

        Multimap<OWLDataPropertyExpression, OWLLiteral> dataValues =
                EntitySearcher.getDataPropertyValues(individual, ontologies);
        if (!dataValues.isEmpty()){
            write("<ul>");
            for (OWLDataPropertyExpression p : dataValues.keySet()){
                write("<li>");
                p.accept(this);
                write("<ul><li>");
                writeOpList(dataValues.get(p), "</li><li>", false);
                write("</ul></li>");
            }
            write("</ul>");
        }

        Multimap<OWLDataPropertyExpression, OWLLiteral> negDataValues =
                EntitySearcher.getNegativeDataPropertyValues(individual, ontologies);
        if (!negDataValues.isEmpty()){
            write("<ul>");

            for (OWLDataPropertyExpression p : negDataValues.keySet()){
                write("<li>not ");
                p.accept(this);
                write("<ul><li>");
                writeOpList(negDataValues.get(p), "</li><li>", false);
                write("</ul></li>");
            }
            write("</ul>");
        }

        Multimap<OWLObjectPropertyExpression, OWLIndividual> objValues =
                EntitySearcher.getObjectPropertyValues(individual, ontologies);
        if (!objValues.isEmpty()){
            write("<ul>");

            for (OWLObjectPropertyExpression p : objValues.keySet()){
                write("<li>");
                p.accept(this);
                write("<ul><li>");
                writeOpList(objValues.get(p), "</li><li>", false);
                write("</ul></li>");
            }
            write("</ul>");

        }

        Multimap<OWLObjectPropertyExpression, OWLIndividual> negbjValues =
                EntitySearcher.getNegativeObjectPropertyValues(individual, ontologies);
        if (!negbjValues.isEmpty()){
            write("<ul>");

            for (OWLObjectPropertyExpression p : negbjValues.keySet()){
                write("<li>not ");
                p.accept(this);
                write("<ul><li>");
                writeOpList(negbjValues.get(p), "</li><li>", false);
                write("</ul></li>");
            }
            write("</ul>");
        }
    }

    private void writeCardinality(OWLCardinalityRestriction desc, String cardinalityType) {
        desc.getProperty().accept(this);
        write(" ");
        writeKeyword(cardinalityType, cardinalityType);
        write(" ");
        write(Integer.toString(desc.getCardinality()));
        write(" ");
        writeOp(desc.getFiller(), true);
    }

    private void writeUnaryPropertyAxiom(OWLUnaryPropertyAxiom axiom, String keyword) {
        writeKeyword(keyword);
        write(" (");
        writeOp(axiom.getProperty(), true);
        write(")");
        writeAnnotations(axiom);
    }

    private String writeFacet(OWLFacet facet) {
        // need to make ranges HTML safe
        if (facet.equals(OWLFacet.MIN_INCLUSIVE)) return "&gt;=";
        else if (facet.equals(OWLFacet.MIN_EXCLUSIVE)) return "&gt;";
        else if (facet.equals(OWLFacet.MAX_INCLUSIVE)) return "&lt;=";
        else if (facet.equals(OWLFacet.MAX_EXCLUSIVE)) return "&lt;";
        return facet.getSymbolicForm();
    }

    private void writeLiteralContents(String literal) {
        boolean writtenExternalRef = false;
        try {
            URI uri = new URI(literal);
            if (uri.isAbsolute()){
                write("<a href='" + uri + "' target='ext_ref'>" + uri + "</a>");
                writtenExternalRef = true;
            }
        }
        catch (URISyntaxException e) {
            // do nothing
        }
        finally{
            if (!writtenExternalRef){
                literal = literal.replace("<", "&lt;");
                literal = literal.replace(">", "&gt;");
                literal = literal.replace("\n", "<br />");
                write(literal);
            }
        }
    }

    private void writeAnnotations(OWLAxiom axiom) {
        final Set<OWLAnnotation> annotations = axiom.getAnnotations();
        if (!annotations.isEmpty()){
            write("<ul>");
            for (OWLAnnotation annot : annotations){
                write("<li>");
                annot.accept(this);
                write("</li>");
            }
            write("</ul>");
        }
    }

    private void writeAssertionAxiom(OWLPropertyAssertionAxiom axiom) {
        axiom.getSubject().accept(this);
        write(" ");
        axiom.getProperty().accept(this);
        write(" ");
        axiom.getObject().accept(this);
        writeAnnotations(axiom);
    }

    private <O extends OWLObject> void writeOpList(Iterable<O> args, String separator, boolean wrap) {
        for (Iterator<O> i = args.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                write(separator);
                if (wrap && indent > 0){
                    write("<br>"); // cannot use <br /> in java browser
                    writeIndent();
                }
            }
        }
    }

    private <O extends OWLObject> void writeKeywordOpList(Iterable<O> args, String keyword, boolean wrap) {
        for (Iterator<O> i = args.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                write(" ");
                writeKeyword(keyword);
                write(" ");
                if (wrap && indent > 0){
                    write("<br>"); // cannot use <br /> in java browser
                    writeIndent();
                }
            }
        }
    }
    private void writeCSSClasses(Set<String> cssClasses) {
        if (!cssClasses.isEmpty()){
            boolean started = false;
            write(" class='");
            for (String cls : cssClasses){
                if (started){
                    write(" ");
                }
                write(cls);
                started = true;
            }
            write("'");
        }
    }
}

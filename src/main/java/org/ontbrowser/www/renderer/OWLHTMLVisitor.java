package org.ontbrowser.www.renderer;

import com.google.common.collect.Multimap;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntax;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLFacet;

import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;
import java.util.stream.Collectors;

public class OWLHTMLVisitor implements OWLObjectVisitor {

    private static final boolean WRITE_AXIOM_ANNOTATION = false;

    // These should match the css class names
    private static final String CSS_ACTIVE_ENTITY = "active-entity";
    private static final String CSS_KEYWORD = "keyword";
    private static final String CSS_ONTOLOGY_URI = "ontology-uri";
    private static final String CSS_SOME = "some";
    private static final String CSS_ONLY = "only";
    private static final String CSS_VALUE = "value";
    private static final String CSS_LITERAL = "literal";

    public static final String EQUIV_CHAR = "&equiv;";
    public static final String SUBCLASS_CHAR = "&sube;";
    public static final String NBSP = "&nbsp;";
    public static final String SPACE = " ";
    public static final String OPEN_PAREN = "(";
    public static final String CLOSE_PAREN = ")";
    public static final String HTML_LIST_START = "<ul>";
    public static final String HTML_LIST_END = "</ul>";
    public static final String HTML_LIST_ITEM_START = "<li>";
    public static final String HTML_LIST_ITEM_END = "</li>";

    // the subset and equivalence symbols can be encoded in HTML
    private static final boolean USE_SYMBOLS = true;

    // the active objects currently on the page (will be highlighted)
    private Set<OWLObject> activeObjects = Collections.emptySet();

    private URLScheme urlScheme;

    private final ShortFormProvider sfProvider;

    private final OntologyIRIShortFormProvider ontologyIriSFProvider;
    private final OWLOntology ont;
    private final OWLEntityFinder finder;

    private PrintWriter out;

    private int indent = 0;

    public OWLHTMLVisitor(
            ShortFormProvider sfProvider,
            OntologyIRIShortFormProvider ontologySFProvider,
            URLScheme urlScheme,
            OWLOntology ont,
            OWLEntityFinder finder) {
        this.sfProvider = sfProvider;
        this.ontologyIriSFProvider = ontologySFProvider;
        this.urlScheme = urlScheme;
        this.ont = ont;
        this.finder = finder;
    }
    public void setURLScheme(URLScheme urlScheme) {
        this.urlScheme = urlScheme;
    }

    public void setActiveObjects(Set<OWLObject> activeObjects) {
        this.activeObjects = activeObjects;
    }

    public void setWriter(PrintWriter out) {
        this.out = out;
    }

    private void write(String s) {
        out.write(s);
    }

    ////////// Ontology

    @Override
    public void visit(OWLOntology ontology) {
        String link = urlScheme.getURLForOWLObject(ontology);
        String cssClass = CSS_ONTOLOGY_URI;

        if (activeObjects.contains(ontology)){
            cssClass = CSS_ACTIVE_ENTITY + SPACE + cssClass;
        }

        write("<a class='" + cssClass + "'");
        String id = getOntologyIdString(ontology);
        write(" href=\"" + link + "\" title='" + id + "'");
        write(">");
        write(ontologyIriSFProvider.getShortForm(ontology));
        write("</a>");
    }

    private String getOntologyIdString(final OWLOntology ont){
        return ont.getOntologyID().getDefaultDocumentIRI().map(IRI::toString)
                .orElseGet(() -> this.getOntologyDocString(ont));
    }

    private String getOntologyDocString(final OWLOntology ont) {
        return ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
    }

    ////////// Entities

    @Override
    public void visit(OWLClass desc) {
        writeOWLEntity(desc, EntityType.CLASS.getPrintName());
    }

    @Override
    public void visit(OWLDataProperty property) {
        writeOWLEntity(property, EntityType.DATA_PROPERTY.getPrintName());
    }

    @Override
    public void visit(OWLObjectProperty property) {
        writeOWLEntity(property, EntityType.OBJECT_PROPERTY.getPrintName());
    }

    @Override
    public void visit(OWLAnnotationProperty property) {
        writeOWLEntity(property, EntityType.ANNOTATION_PROPERTY.getPrintName());
    }

    @Override
    public void visit(OWLNamedIndividual individual) {
        writeOWLEntity(individual, EntityType.NAMED_INDIVIDUAL.getPrintName());
    }

    @Override
    public void visit(OWLDatatype datatype) {
        writeOWLEntity(datatype, EntityType.DATATYPE.getPrintName());
    }

    @Override
    public void visit(IRI iri) {
        Set<? extends OWLEntity> entities = finder.getOWLEntities(iri, ont);
        if (entities.isEmpty()){
            writeIRIWithBoldFragment(iri, iri.getShortForm());
        }
        else if (entities.size() == 1){
            entities.iterator().next().accept(this);
        }
        else {
            boolean started = false;
            for (OWLEntity entity : entities) {
                if (started) {
                    write(", ");
                } else {
                    started = true;
                }
                entity.accept(this);
                write(" (");
                write(entity.getEntityType().getPluralPrintName());
                write(" )");
            }
        }
    }

    @Override
    public void visit(OWLAnonymousIndividual individual) {
        writeAnonymousIndividual(individual);
    }

    ///////// Anonymous classes

    @Override
    public void visit(OWLObjectSomeValuesFrom desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.SOME.toString(), CSS_SOME);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLObjectAllValuesFrom desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.ONLY.toString(), CSS_ONLY);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLObjectHasValue desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.VALUE.toString(), CSS_VALUE);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLObjectMinCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MIN.toString());
    }

    @Override
    public void visit(OWLObjectExactCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.EXACTLY.toString());
    }

    @Override
    public void visit(OWLObjectMaxCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MAX.toString());
    }

    @Override
    public void visit(OWLObjectComplementOf desc) {
        writeKeyword(ManchesterOWLSyntax.NOT.toString());
        write(SPACE);
        writeOp(desc.getOperand(), false);
    }

    @Override
    public void visit(OWLObjectHasSelf desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.SELF.toString());
    }

    @Override
    public void visit(OWLObjectIntersectionOf desc) {
        writeKeywordOpList(orderOps(desc.getOperands()), ManchesterOWLSyntax.AND.toString(), true);
    }

    @Override
    public void visit(OWLObjectUnionOf desc) {
        writeKeywordOpList(orderOps(desc.getOperands()), ManchesterOWLSyntax.OR.toString(), false);
    }

    @Override
    public void visit(OWLObjectOneOf desc) {
        write("{");
        writeOpList(desc.getIndividuals(), ", ");
        write("}");
    }

    @Override
    public void visit(OWLDataOneOf desc) {
        write("{");
        writeOpList(desc.getValues(), ", ");
        write("}");
    }

    @Override
    public void visit(OWLDataSomeValuesFrom desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.SOME.toString(), CSS_SOME);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLDataAllValuesFrom desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.ONLY.toString(), CSS_ONLY);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLDataHasValue desc) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.VALUE.toString(), CSS_VALUE);
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    @Override
    public void visit(OWLDataMinCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MIN.toString());
    }

    @Override
    public void visit(OWLDataExactCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.EXACTLY.toString());
    }

    @Override
    public void visit(OWLDataMaxCardinality desc) {
        writeCardinality(desc, ManchesterOWLSyntax.MAX.toString());
    }

    @Override
    public void visit(OWLDatatypeRestriction node) {
        node.getDatatype().accept(this);
        write(SPACE);
        write(" [");
        writeOpList(node.getFacetRestrictions(), ", ");
        write("]");
    }

    @Override
    public void visit(OWLFacetRestriction node) {
        writeKeyword(writeFacet(node.getFacet()));
        node.getFacetValue().accept(this);
    }

    @Override
    public void visit(OWLDataComplementOf node) {
        writeKeyword(ManchesterOWLSyntax.NOT.toString());
        write(SPACE);
        writeOp(node.getDataRange(), true);
    }

    @Override
    public void visit(OWLDataIntersectionOf owlDataIntersectionOf) {
        writeKeywordOpList(owlDataIntersectionOf.getOperands(), ManchesterOWLSyntax.AND.toString(), true);
    }

    @Override
    public void visit(OWLDataUnionOf owlDataUnionOf) {
        writeKeywordOpList(owlDataUnionOf.getOperands(), ManchesterOWLSyntax.OR.toString(), false);
    }

    ////////// Properties

    @Override
    public void visit(OWLObjectInverseOf property) {
        writeKeyword(ManchesterOWLSyntax.INVERSE_OF.toString());
        write(SPACE);
        writeOp(property.getInverse(), true);
    }

    @Override
    public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.FUNCTIONAL.toString());
    }

    @Override
    public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.INVERSE_FUNCTIONAL.toString());
    }

    @Override
    public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.SYMMETRIC.toString());
    }

    @Override
    public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.TRANSITIVE.toString());
    }

    @Override
    public void visit(OWLAsymmetricObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.ASYMMETRIC.toString());
    }

    @Override
    public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.REFLEXIVE.toString());
    }

    @Override
    public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.IRREFLEXIVE.toString());
    }

    @Override
    public void visit(OWLObjectPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.DOMAIN.toString());
        write(SPACE);
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLObjectPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(SPACE);
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        writeOp(axiom.getFirstProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.INVERSE.toString());
        write(SPACE);
        writeOp(axiom.getSecondProperty(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLHasKeyAxiom axiom) {
        writeOp(axiom.getClassExpression(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.HAS_KEY.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getPropertyExpressions(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDatatypeDefinitionAxiom axiom) {
        axiom.getDatatype().accept(this);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.EQUIVALENT_TO.toString());
        write(SPACE);
        axiom.getDataRange().accept(this);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(SWRLRule swrlRule) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLClassAtom swrlClassAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLDataRangeAtom swrlDataRangeAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLObjectPropertyAtom swrlObjectPropertyAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLDataPropertyAtom swrlDataPropertyAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLBuiltInAtom swrlBuiltInAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLVariable swrlVariable) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLIndividualArgument swrlIndividualArgument) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLLiteralArgument swrlLiteralArgument) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLSameIndividualAtom swrlSameIndividualAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(SWRLDifferentIndividualsAtom swrlDifferentIndividualsAtom) {
        // SWRL not supported
    }

    @Override
    public void visit(OWLSubPropertyChainOfAxiom axiom) {
        writeKeywordOpList(axiom.getPropertyChain(), "o", false);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(SPACE);
        writeOp(axiom.getSuperProperty(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDataPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.DOMAIN.toString());
        write(SPACE);
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDataPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(SPACE);
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLFunctionalDataPropertyAxiom axiom) {
        writeUnaryPropertyAxiom(axiom, ManchesterOWLSyntax.FUNCTIONAL.toString());
        writeAnnotations(axiom);
    }

    ////////// Annotations

    @Override
    public void visit(OWLAnnotationAssertionAxiom axiom) {
        final OWLAnnotationSubject subject = axiom.getSubject();
        // extract the entities with this IRI
        if (subject instanceof IRI iri){
            Set<OWLEntity> entities = ont.getEntitiesInSignature(iri, Imports.INCLUDED);
            if (!entities.isEmpty()){
                boolean started = false;
                for (OWLEntity entity : entities){
                    if (started){
                        write(NBSP);
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
        write(NBSP);
        axiom.getAnnotation().accept(this);
        write(NBSP);
        writeAnnotations(axiom); // in theory, you could annotate the annotation axioms !!
    }

    @Override
    public void visit(OWLSubAnnotationPropertyOfAxiom axiom) {
        writeOp(axiom.getSubProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(SPACE);
        writeOp(axiom.getSuperProperty(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLAnnotationPropertyDomainAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(SPACE);
        writeOp(axiom.getDomain(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLAnnotationPropertyRangeAxiom axiom) {
        writeOp(axiom.getProperty(), true);
        write(SPACE);
        writeKeyword(ManchesterOWLSyntax.RANGE.toString());
        write(SPACE);
        writeOp(axiom.getRange(), true);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLAnnotation annotation) {
        annotation.getProperty().accept(this);
        write(SPACE);
        annotation.getValue().accept(this);
    }

    // OWLAPI v3.1
    @Override
    public void visit(OWLLiteral node) {
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
            if (node.hasLang()){
                final String lang = node.getLang();
                write(" <span>@" + lang + "</span>");
            }
            else if (!dt.isString()) {
                write("^^");
                dt.accept(this);
            }
        }
    }

    /////////// Axioms

    @Override
    public void visit(OWLEquivalentClassesAxiom axiom) {
        writeEquivalence(orderOps(axiom.getClassExpressions()), axiom);
    }

    @Override
    public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
        writeEquivalence(axiom.getProperties(), axiom);
    }

    @Override
    public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
        writeEquivalence(axiom.getProperties(), axiom);
    }

    @Override
    public void visit(OWLSameIndividualAxiom axiom) {
        writeKeywordOpList(axiom.getIndividuals(), ManchesterOWLSyntax.SAME_AS.toString(), false);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLSubClassOfAxiom axiom) {
        axiom.getSubClass().accept(this);
        write(SPACE);
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUBCLASS_OF.toString());
        write(SPACE);
        axiom.getSuperClass().accept(this);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLSubObjectPropertyOfAxiom axiom) {
        axiom.getSubProperty().accept(this);
        write(SPACE);
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(SPACE);
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLSubDataPropertyOfAxiom axiom) {
        axiom.getSubProperty().accept(this);
        write(SPACE);
        writeKeyword(USE_SYMBOLS ? SUBCLASS_CHAR : ManchesterOWLSyntax.SUB_PROPERTY_OF.toString());
        write(SPACE);
        axiom.getSuperProperty().accept(this);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDisjointClassesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_CLASSES.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getClassExpressions(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_PROPERTIES.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getProperties(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDisjointDataPropertiesAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_PROPERTIES.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getProperties(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDifferentIndividualsAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DIFFERENT_INDIVIDUALS.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getIndividuals(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDisjointUnionAxiom axiom) {
        writeKeyword(ManchesterOWLSyntax.DISJOINT_UNION_OF.toString());
        write(SPACE);
        write(OPEN_PAREN);
        writeOpList(axiom.getClassExpressions(), ", ");
        write(CLOSE_PAREN);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLDeclarationAxiom axiom) {
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

    @Override
    public void visit(OWLClassAssertionAxiom axiom) {
        axiom.getIndividual().accept(this);
        write(": ");
        axiom.getClassExpression().accept(this);
        writeAnnotations(axiom);
    }

    @Override
    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    @Override
    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    @Override
    public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    @Override
    public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
        writeAssertionAxiom(axiom);
    }

    private void writeEquivalence(Collection<? extends OWLObject> objects, OWLAxiom axiom) {
        String equiv = USE_SYMBOLS ? EQUIV_CHAR : ManchesterOWLSyntax.EQUIVALENT_TO.toString();
        writeKeywordOpList(objects, equiv, false);
        writeAnnotations(axiom);
    }

    // just make sure a named class is first if there is one
    private List<OWLClassExpression> orderOps(Set<OWLClassExpression> ops) {
        List<OWLClassExpression> orderedOps = new ArrayList<>(ops);
        orderedOps.sort((d1, d2) -> {
            if (d1 instanceof OWLClass) {
                return -1;
            } else if (d2 instanceof OWLClass) {
                return 1;
            }
            return 0;
        });
        return orderedOps;
    }

    // Make names line breakable on underscore
    private String renderBreakableName(OWLEntity entity){
        return sfProvider.getShortForm(entity).replace("_", "_<wbr>");
    }

    // Make string line breakable on / and #
    private String makeBreakable(String s) {
        return s.replaceAll("/(?=[^/])", "/<wbr>").replace("#", "#<wbr>");
    }

    private void writeIRIWithBoldFragment(IRI iri, String shortForm) {
        // Encourage wrapping on / instead of other characters
        final String fullURI = makeBreakable(iri.toString());

        int index = fullURI.lastIndexOf(shortForm);
        if (index == -1){
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
            write(OPEN_PAREN);
            if (wrap && op instanceof OWLObjectIntersectionOf){
                indent++;
                write("<br>");
                writeIndent();
            }
            op.accept(this);
            if (wrap && op instanceof OWLObjectIntersectionOf){
                indent--;
            }
            write(CLOSE_PAREN);
        }
    }

    private void writeIndent() {
        for (int i=0; i<indent; i++){
            write("&nbsp;&nbsp;&nbsp;&nbsp;");
        }
    }

    private void writeOWLEntity(OWLEntity entity, String cssClass) {
        final URI uri = entity.getIRI().toURI();

        String renderedName = renderBreakableName(entity);

        Set<String> cssClasses = new HashSet<>();
        cssClasses.add(cssClass);

        if (activeObjects.contains(entity)) {
            cssClasses.add(CSS_ACTIVE_ENTITY);
        }

        final String urlForTarget = urlScheme.getURLForOWLObject(entity);
        write("<a href=\"" + urlForTarget + "\"");
        writeCSSClasses(cssClasses);
        write(" title=\"" + uri + "\">");
        write(renderedName);
        write("</a>");
    }

    private void writeAnonymousIndividual(OWLAnonymousIndividual individual) {
        writeTypes(individual);
        writeAnnotations(individual);
        writeDataPropertyValues(individual);
        writeNegativeDataPropertyValues(individual);
        writeObjectPropertyValues(individual);
        writeNegativeObjectPropertyValues(individual);
    }

    private void writeNegativeObjectPropertyValues(OWLAnonymousIndividual individual) {
        Multimap<OWLObjectPropertyExpression, OWLIndividual> negObjValues =
                EntitySearcher.getNegativeObjectPropertyValues(individual, ont.importsClosure());
        if (!negObjValues.isEmpty()){
            write(HTML_LIST_START);

            for (OWLObjectPropertyExpression p : negObjValues.keySet()){
                write("<li>not ");
                p.accept(this);
                write("<ul><li>");
                writeOpList(negObjValues.get(p), "</li><li>");
                write("</ul></li>");
            }
            write(HTML_LIST_END);
        }
    }

    private void writeObjectPropertyValues(OWLAnonymousIndividual individual) {
        Multimap<OWLObjectPropertyExpression, OWLIndividual> objValues =
                EntitySearcher.getObjectPropertyValues(individual, ont.importsClosure());
        if (!objValues.isEmpty()){
            write(HTML_LIST_START);

            for (OWLObjectPropertyExpression p : objValues.keySet()){
                write(HTML_LIST_ITEM_START);
                p.accept(this);
                write("<ul><li>");
                writeOpList(objValues.get(p), "</li><li>");
                write("</ul></li>");
            }
            write(HTML_LIST_END);

        }
    }

    private void writeNegativeDataPropertyValues(OWLAnonymousIndividual individual) {
        Multimap<OWLDataPropertyExpression, OWLLiteral> negDataValues =
                EntitySearcher.getNegativeDataPropertyValues(individual, ont.importsClosure());
        if (!negDataValues.isEmpty()){
            write(HTML_LIST_START);

            for (OWLDataPropertyExpression p : negDataValues.keySet()){
                write("<li>not ");
                p.accept(this);
                write("<ul><li>");
                writeOpList(negDataValues.get(p), "</li><li>");
                write("</ul></li>");
            }
            write(HTML_LIST_END);
        }
    }

    private void writeDataPropertyValues(OWLAnonymousIndividual individual) {
        Multimap<OWLDataPropertyExpression, OWLLiteral> dataValues =
                EntitySearcher.getDataPropertyValues(individual, ont.importsClosure());
        if (!dataValues.isEmpty()){
            write(HTML_LIST_START);
            for (OWLDataPropertyExpression p : dataValues.keySet()){
                write(HTML_LIST_ITEM_START);
                p.accept(this);
                write(HTML_LIST_START);
                write(HTML_LIST_ITEM_START);
                writeOpList(dataValues.get(p), HTML_LIST_ITEM_END + HTML_LIST_ITEM_START);
                write(HTML_LIST_ITEM_END);
                write(HTML_LIST_END);
                write(HTML_LIST_ITEM_END);
            }
            write(HTML_LIST_END);
        }
    }

    private void writeAnnotations(OWLAnonymousIndividual individual) {
        Set<OWLAnnotation> annotations = EntitySearcher.getAnnotations(individual, ont.importsClosure(), null)
                .collect(Collectors.toSet());

        if (!annotations.isEmpty()){
            write(HTML_LIST_START);
            for (OWLAnnotation a : annotations){
                write(HTML_LIST_ITEM_START);
                a.accept(this);
                write(HTML_LIST_ITEM_END);
            }
            write(HTML_LIST_END);
        }
    }

    private void writeTypes(OWLAnonymousIndividual individual) {
        final Collection<OWLClassExpression> types =
                EntitySearcher.getTypes(individual, ont.importsClosure()).toList();

        if (!types.isEmpty()){
            writeOpList(types, ", ");
        }
    }

    private void writeCardinality(OWLCardinalityRestriction<? extends OWLPropertyRange> desc, String cardinalityType) {
        desc.getProperty().accept(this);
        write(SPACE);
        writeKeyword(cardinalityType, cardinalityType);
        write(SPACE);
        write(Integer.toString(desc.getCardinality()));
        write(SPACE);
        writeOp(desc.getFiller(), true);
    }

    private void writeUnaryPropertyAxiom(OWLUnaryPropertyAxiom<? extends OWLPropertyExpression> axiom, String keyword) {
        writeKeyword(keyword);
        write(" (");
        writeOp(axiom.getProperty(), true);
        write(CLOSE_PAREN);
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
                write("<a href='" + uri + "' target='ext_ref'>" + makeBreakable(uri.toString()) + "</a>");
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
        if (WRITE_AXIOM_ANNOTATION && axiom.isAnnotated()){
            final Set<OWLAnnotation> annotations = axiom.getAnnotations();
            write(HTML_LIST_START);
            for (OWLAnnotation annot : annotations){
                write(HTML_LIST_ITEM_START);
                annot.accept(this);
                write(HTML_LIST_ITEM_END);
            }
            write(HTML_LIST_END);
        }
    }

    private void writeAssertionAxiom(OWLPropertyAssertionAxiom<? extends OWLPropertyExpression, ? extends OWLPropertyAssertionObject> axiom) {
        axiom.getSubject().accept(this);
        write(SPACE);
        axiom.getProperty().accept(this);
        write(SPACE);
        axiom.getObject().accept(this);
        writeAnnotations(axiom);
    }

    private <O extends OWLObject> void writeOpList(Iterable<O> args, String separator) {
        for (Iterator<O> i = args.iterator(); i.hasNext();) {
            i.next().accept(this);
            if (i.hasNext()){
                write(separator);
            }
        }
    }

    private <O extends OWLObject> void writeKeywordOpList(Iterable<O> args, String keyword, boolean wrap) {
        for (Iterator<O> i = args.iterator(); i.hasNext();) {
            O o = i.next();
            if (o.isNamed()
                    || o instanceof OWLObjectOneOf
                    || o instanceof OWLObjectComplementOf
                    || o instanceof OWLDataComplementOf) {
                o.accept(this);
            }
            else {
                write(OPEN_PAREN);
                o.accept(this);
                write(CLOSE_PAREN);
            }
            if (i.hasNext()){
                if (wrap && indent > 0){
                    write("<br>"); // cannot use <br /> in java browser
                    writeIndent();
                }
                else {
                    write(SPACE);
                }
                writeKeyword(keyword);
                write(SPACE);
            }
        }
    }
    private void writeCSSClasses(Set<String> cssClasses) {
        if (!cssClasses.isEmpty()){
            boolean started = false;
            write(" class='");
            for (String cls : cssClasses){
                if (started){
                    write(SPACE);
                }
                write(cls);
                started = true;
            }
            write("'");
        }
    }

}

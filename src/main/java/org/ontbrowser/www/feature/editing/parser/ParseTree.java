package org.ontbrowser.www.feature.editing.parser;

import org.ontbrowser.www.feature.editing.parser.matcher.*;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntax;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;

import java.util.*;
import java.util.function.Function;

public class ParseTree implements OWLObjectProvider {

    public static final String PREFIX_SUGAR = ":";
    private OWLDataFactory df;
    private MyTokenizer tokenizer;
    private OWLEntityChecker checker;
    private final LinkedHashMap<String, AbstractParseMatcher> matchers = new LinkedHashMap<>();
    private OWLAxiom axiom;

    private Function<OWLObjectProvider, OWLAxiom> creator;
    private ParseTree parent = null;

    private ParseTree() {
    }

    public ParseTree(final String s, final OWLEntityChecker checker, OWLDataFactory df) {
        this.tokenizer = new MyTokenizer(s);
        this.checker = checker;
        this.df = df;
    }

    public static ParseTree branch() {
        return new ParseTree();
    }

    public ParseTree expect(String key, AbstractParseMatcher m) throws ParserException {
        matchers.put(key, m);
        return this;
    }

    public ParseTree expectEither(ParseTree... branches) throws ParserException {
        Arrays.stream(branches).forEach(branch -> branch.parent = ParseTree.this);
        return expect("", new BranchingMatcher(branches));
    }

    public ParseTree create(Function<OWLObjectProvider, OWLAxiom> o) {
        this.creator = o;
        return this;
    }

    public ParseTree expectIndividual(String a) {
        return expect(a, new IndividualMatcher());
    }

    public ParseTree expectClass(String a) {
        return expect(a, new ClassMatcher());
    }

    public ParseTree expectClassExpression(String a) {
        return expect(a, new ClassExpressionMatcher());
    }

    public ParseTree expectObjectProperty(String p) {
        return expect(p, new ObjPropMatcher());
    }

    public ParseTree expectObjectPropertyExpression(String p) {
        return expect(p, new ObjPropExpressionMatcher());
    }

    public ParseTree expectDataProperty(String p) {
        return expect(p, new DataPropMatcher());
    }

    public ParseTree expectDataPropertyExpression(String p) {
        return expect(p, new DataPropExpressionMatcher());
    }

    public ParseTree expectAnnotationProperty(String p) {
        return expect(p, new AnnotationPropMatcher());
    }

    public ParseTree expectKeyword(ManchesterOWLSyntax manchesterOWLSyntax) {
        return expect(manchesterOWLSyntax.toString(), new KeywordMatcher(manchesterOWLSyntax));
    }

    public ParseTree expectPrefixKeyword(ManchesterOWLSyntax manchesterOWLSyntax) {
        return expect(manchesterOWLSyntax.toString() + PREFIX_SUGAR, new KeywordMatcher(manchesterOWLSyntax, PREFIX_SUGAR));
    }

    public ParseTree expectLiteral(String key) {
        return expect(key, new LiteralMatcher());
    }


    public ParseTree expectDatatype(String key) {
        return expect(key, new DatatypeMatcher());
    }

    public ParseTree expectSugar(String key) {
        return expect(key, new SugarMatcher(key));
    }

    public ParseTree expectClassList(String key, String separator) {
        return expect(key, new ClassListMatcher(separator));
    }

    public ParseTree expectIndividualsList(String key, String separator) {
        return expect(key, new IndividualListMatcher(separator));
    }

    @Override
    public OWLObjectPropertyExpression objPropExpr(String key) {
        return getMatcherForKey(key, OWLObjectPropertyExpression.class).getObjectPropertyExpression();
    }

    @Override
    public OWLObjectProperty objProp(String key) {
        return getMatcherForKey(key, OWLObjectProperty.class).getObjectProperty();
    }

    @Override
    public OWLDataPropertyExpression dataPropExpr(String key) {
        return getMatcherForKey(key, OWLDataPropertyExpression.class).getDataPropertyExpression();
    }

    @Override
    public OWLDataProperty dataProp(String key) {
        return getMatcherForKey(key, OWLDataProperty.class).getDataProperty();
    }

    @Override
    public OWLAnnotationProperty annotProp(String key) {
        return getMatcherForKey(key, OWLAnnotationProperty.class).getAnnotationProperty();
    }

    @Override
    public OWLNamedIndividual ind(String key) {
        return getMatcherForKey(key, OWLNamedIndividual.class).getIndividual();
    }

    @Override
    public OWLClass cls(String key) {
        return getMatcherForKey(key, OWLClass.class).getOWLClass();
    }

    private <T> AbstractParseMatcher<T> getMatcherForKey(String key, Class<T> clz) {
        try {
            // TODO type check
            return matchers.get(key);
        }
        catch (NullPointerException e) {
            throw new RuntimeException("Cannot find binding for " + key, e);
        }
    }

    @Override
    public OWLClassExpression clsExpr(String key) {
        return getMatcherForKey(key, OWLClassExpression.class).getOWLClassExpression();
    }

    @Override
    public OWLDatatype datatype(String key) {
        return getMatcherForKey(key, OWLDatatype.class).getDatatype();
    }

    @Override
    public <T> List<T> list(String key, Class<T> type) {
        return getMatcherForKey(key, type).getObjectList(type);
    }

    @Override
    public OWLLiteral lit(String key) {
        return getMatcherForKey(key, OWLLiteral.class).getLiteral();
    }

    public OWLAxiom getAxiom() {
        if (axiom == null) {
            // make a copy as branches may modify the content
            new LinkedHashMap<>(matchers).forEach( (k, m) -> m.check(tokenizer, checker, df));
        }
        axiom = creator.apply(this);
        return axiom;
    }

    public <T> ParseTree expectList(String key, ManchesterOWLSyntax separator, AbstractParseMatcher<T> elementMatcher) {
        return expectList(key, separator.keyword(), elementMatcher);
    }

    public <T> ParseTree expectList(String key, String separator, AbstractParseMatcher<T> elementMatcher) {
        return expect(key, new ListMatcher<T>(separator, elementMatcher));
    }

    private class ListMatcher<T> extends AbstractParseMatcher<T> {

        private final String separator;
        private final AbstractParseMatcher<T> elementMatcher;

        private final List<T> results = new ArrayList<>();

        public ListMatcher(String separator, AbstractParseMatcher<T> elementMatcher) {
            this.separator = separator;
            this.elementMatcher = elementMatcher;
        }

        public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
            try {
                elementMatcher.check(tokenizer, checker, df);
                results.add(elementMatcher.get());
                new SugarMatcher(separator).check(tokenizer, checker, df);
            }
            catch(ParserException e) {
                if (results.isEmpty()) { // if we haven't found at least one element then rethrow
                    throw e;
                }
            }
        }
    }

    private class BranchingMatcher<T> extends AbstractParseMatcher<T> {
        private final List<ParseTree> branches;

        public BranchingMatcher(ParseTree... branches) {
            this.branches = List.of(branches);
        }

        @Override
        public void check(MyTokenizer tokenizer, OWLEntityChecker checker, OWLDataFactory df) throws ParserException {
            List<ParserException> fails = new ArrayList<>();
            for (ParseTree branch : branches) {
                try {
                    branch.tokenizer = new MyTokenizer(branch.parent.tokenizer); // clone state of parent tokenizer for each branch
                    branch.checker = branch.parent.checker;
                    branch.df = branch.parent.df;

                    // run all matchers in the branch (make a copy first because of nesting)
                    new LinkedHashMap<>(branch.matchers).forEach( (k, m) -> m.check(branch.tokenizer, branch.checker, branch.df));

                    // if no parse errors

                    // copy the successful matchers to the parent
                    branch.parent.matchers.putAll(branch.matchers);

                    // along with the builder which will eventually call this creator
                    branch.parent.creator = branch.creator;

                    return; // No need to try other branches if successful
                }
                catch(ParserException e) {
                    fails.add(e);
                }
            }
            // if all branches below this failed, fail this branch
            if (fails.size() == branches.size()) {
                throw merge(getBestFails(fails));
            }
        }
    }

    private List<ParserException> getBestFails(List<ParserException> fails) {
        int maxFailDepth = maxDepth(fails);
        return fails.stream().filter(f -> f.getStartPos() >= maxFailDepth).toList();
    }

    private ParserException merge(List<ParserException> fails) {

        ParserException representativeException = fails.get(0);
        if (fails.size() == 1) {
            return representativeException;
        }

        List<String> tokens = representativeException.getTokenSequence();
        int startPos = representativeException.getStartPos();
        int line = representativeException.getLineNumber();
        int column = representativeException.getColumnNumber();

        boolean ont = false;
        boolean cls = false;
        boolean oProp = false;
        boolean dProp = false;
        boolean ind = false;
        boolean dt = false;
        boolean aProp = false;
        boolean inte = false;
        Set<String> keys = new HashSet<>();

        for (ParserException f : fails) {
            if (f.isOntologyNameExpected()) ont = true;
            if (f.isClassNameExpected()) cls = true;
            if (f.isObjectPropertyNameExpected()) oProp = true;
            if (f.isDataPropertyNameExpected()) dProp = true;
            if (f.isIndividualNameExpected()) ind = true;
            if (f.isDatatypeNameExpected()) dt = true;
            if (f.isAnnotationPropertyNameExpected()) aProp = true;
            if (f.isIntegerExpected()) inte = true;
            keys.addAll(f.getExpectedKeywords());
        }

        return new ParserException(tokens, startPos, line, column, ont, cls, oProp, dProp, ind, dt, aProp, inte, keys);
    }

    private int maxDepth(List<ParserException> fails) {
        int max = 0;
        for (ParserException f : fails) {
            // if we use the tokenSequence this will not work for axioms with Class Expressions
            int size = f.getStartPos();
            if (size > max) {
                max = size;
            }
        }
        return max;
    }
}

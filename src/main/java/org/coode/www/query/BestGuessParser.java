package org.coode.www.query;

/**
 * Tries to parse a normal Manchester Syntax expression
 * and if it fails, reverts to QuickDescription syntax
 */
public class BestGuessParser{// implements OWLClassExpressionParser {
//
//    private OWLObjectProperty topProp;
//    private OWLServer kit;
//
//    private ManchesterOWLSyntaxParser manParser;
//    private QuickDescriptionParser qdParser;
//
//    public BestGuessParser(OWLServer kit, OWLObjectProperty topProp) {
//        this.topProp = topProp;
//        this.kit = kit;
//    }
//
//    public OWLClassExpression parse(String str) throws ParseException {
//
//        if (manParser == null){
//            manParser = new ManchesterOWLSyntaxParser(kit);
//        }
//
//        OWLClassExpression descr = null;
//
//        try{
//            descr = manParser.parse(str);
//        }
//        catch(ParseException e){
//            if (qdParser == null){
//                qdParser = new QuickDescriptionParser(kit, topProp);
//            }
//            descr = qdParser.parse(str);
//        }
//
//        return descr;
//    }
}

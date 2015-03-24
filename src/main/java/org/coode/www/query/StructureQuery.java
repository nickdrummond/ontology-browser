package org.coode.www.query;

/**
 * Attempts to find "sensible" rather than logically correct answers to the query.
 * Could be thought of as "Which X might reasonably have these features?"
 * Or "Which classes could contain SOME individuals for which this is true?"
 *
 * eg
 *
 * Topping and isIngredientOf some FruttiDiMare
 *  logically, no ingredients are always ingredients of the Frutti Di MAre pizza
 *  realistically, Frutti Di Mare hasToppings MixedSeafood, Tomato and Garlic
 *  this is what we wish to return -
 *
 *
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 13, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class StructureQuery {
//
//    private static final Logger logger = Logger.getLogger(StructureQuery.class.getName());
//
//    private Set<OWLEntity> results = new HashSet<OWLEntity>();
//
//    private MyOWLClassExpressionVisitorAdapter descrVisitor;
//
//    private OWLClass baseClass;
//
//    private OWLServer kit;
//
//    private OWLClassReasoner r;
//
//    public StructureQuery(String query, String syntax, OWLServer kit) throws Exception{
//
//        this.kit = kit;
//
//        OWLClassExpressionParser parser = kit.getClassExpressionParser(syntax);
//
//        //this.r = session.getOntologyServer().getOWLClassReasoner();
//
//        descrVisitor = new MyOWLClassExpressionVisitorAdapter();
//
//        // @@TODO need to test thoroughly with multiple conjuncts
//        for (String token: query.split(ServerConstants.INTERSECTION)){
//            descrVisitor.reset();
//
//            OWLClassExpression descr = parser.parse(token.trim());
//            descr.accept(descrVisitor);
//
//            // merge the values into the results performing AND operation
//            if (descrVisitor.getVisitedRestriction()){
//                Set<OWLEntity> newValues = descrVisitor.getValues();
//                if (newValues.size() > 0){
//                    if (results.size() > 0){
//                        for (Iterator<OWLEntity> i = results.iterator(); i.hasNext();){
//                            OWLEntity result = i.next();
//                            if (!newValues.contains(result)){
//                                i.remove();
//                            }
//                        }
//                    }
//                    else{
//                        results.addAll(newValues);
//                    }
//                }
//                else{
//                    results.clear();
//                    break;
//                }
//            }
//        }
//    }
//
//    public Set<OWLEntity> getResults() {
//        return results;
//    }
//
//    /**
//     * Get the some value restrictions on a given class, including the inherited restrictions
//     */
//    private Set<OWLObjectSomeValuesFrom> getSomeHasValues(OWLClass aClass) {
//
//        Set<OWLObjectSomeValuesFrom> results = new HashSet<OWLObjectSomeValuesFrom>();
//
//        try {
//
//            Set<OWLClass> ancestors = ModelUtil.filterClasses(r.getAncestorClasses(aClass), kit);
//            ancestors.add(aClass);
//
//            for (OWLClass ancestor : ancestors) {
//                for (OWLOntology ont : kit.getOntologies()){
//                    for (OWLClassExpression s: ancestor.getSuperClasses(ont)){
//                        if (s instanceof OWLObjectSomeValuesFrom) {
//                            results.add((OWLObjectSomeValuesFrom) s);
//                        }
//                    }
//                }
//            }
//        }
//        catch (OWLException e) {
//            logger.error("Caught Exception: ", e);
//        }
//
//        return results;
//    }
//
//    /**
//     * Get the some value restrictions on a given class and its subclasses
//     */
//    private Set<OWLObjectSomeValuesFrom> getSubclassSomeHasValues(OWLClass aClass) {
//
//        Set<OWLObjectSomeValuesFrom> results = new HashSet<OWLObjectSomeValuesFrom>();
//
//        try {
//
//            Set<OWLClass> descendents = ModelUtil.filterClasses(r.getDescendantClasses(aClass), kit);
//
//            for (OWLClass descendent : descendents) {
//                for (OWLOntology ont : kit.getOntologies()){
//                    for (OWLClassExpression s: descendent.getSuperClasses(ont)){
//                        if (s instanceof OWLObjectSomeValuesFrom) {
//                            results.add((OWLObjectSomeValuesFrom) s);
//                        }
//                    }
//                }
//            }
//        }
//        catch (OWLReasonerException e) {
//            logger.error("Caught Exception: ", e);
//        }
//
//        return results;
//    }
//
//    /**
//     * Return a set of (named) fillers for restrictions along the given inverseProps
//     */
//    private Set<OWLEntity> getSomeToAllRelations(OWLClass cls, OWLObjectPropertyExpression inverseProp) {
//
//        Set<OWLEntity> inverseFillers = new HashSet<OWLEntity>();
//
//        Set<OWLObjectSomeValuesFrom> restrs = getSomeHasValues(cls);
//        restrs.addAll(getSubclassSomeHasValues(cls));
//
//        for (OWLObjectSomeValuesFrom restr: restrs){
//
//            OWLObjectPropertyExpression restrictedProp = restr.getProperty();
//
//            if (restrictedProp instanceof OWLObjectProperty){
//
//                OWLClassExpression filler = restr.getFiller();
//
//                if (filler instanceof OWLClass){
//
////                    if (r.isSubClassOf(filler, baseClass)){
////
////                        Set<OWLPropertyExpression> rPropsupers = r.getSuperProperties(restrictedProp);
////                        rPropsupers.add(restrictedProp);
////
////                        for (OWLObjectProperty hasIngredientProp : inverseProps){
////                            // if the restricted property is a sub property of this inverse
////                            if (rPropsupers.contains(hasIngredientProp)){
////                                inverseFillers.add((OWLClass)filler);
////                            }
////                        }
////                    }
//                }
//            }
//        }
//        return inverseFillers;
//    }
//    //
//    private class MyOWLClassExpressionVisitorAdapter extends OWLObjectVisitorAdapter {
//        private Set<OWLEntity> accumulator = new HashSet<OWLEntity>();
//        private boolean visitedRestriction = false;
//        private OWLOntologyManager mngr;
//
//        public void setOntology(OWLOntologyManager mngr){
//            this.mngr = mngr;
//        }
//
//        public Set<OWLEntity>getValues(){
//            return accumulator;
//        }
//
//        public void reset(){
//            accumulator.clear();
//            visitedRestriction = false;
//        }
//
//        public boolean getVisitedRestriction(){
//            return visitedRestriction;
//        }
//
//        public void visit(OWLClass owlClass) {
//            baseClass = owlClass;
//
//            logger.debug("baseClass = " + baseClass);
//        }
//
//        public void visit(OWLObjectSomeValuesFrom someRestr) {
//            visitedRestriction = true;
//
//            OWLObjectPropertyExpression prop = someRestr.getProperty();
//
//            OWLObjectPropertyExpression inverse = mngr.getOWLDataFactory().getOWLObjectPropertyInverse(prop);
//
//            OWLClassExpression filler = someRestr.getFiller();
//
//            if (filler instanceof OWLClass){
//                Set<OWLEntity> fillersOfInversRestrictions = getSomeToAllRelations((OWLClass)filler, inverse);
//                accumulator.addAll(fillersOfInversRestrictions);
//            }
//        }
//    }
}


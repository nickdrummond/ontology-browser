package org.coode.www.query;

/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 20, 2007<br><br>
 *
 * This description generator should take a string "Abcde Fghijk Lmnop ..."
 * A space delimited list of classnames.
 * The parser generates a description of the form:
 *
 * Abcde and (isRelatedTo some Fghijk) and (p some Lmnop) and ...
 *
 * In the general case, the top-level property isRelatedTo is used to link to the features.
 * If possible this is more specialised to a property with Abcde in the domain and the filler class
 * in the range.
 *
 * The most specific single property should be returned.
 * Where a single appropriate property cannot be found, a common parent should be used.
 *
 */
public class QuickDescriptionParser{// implements OWLClassExpressionParser {
//
//    private static final Logger logger = Logger.getLogger(QuickDescriptionParser.class.getName());
//
//    private OWLServer kit;
//    private OWLObjectProperty defaultProp;
//
//    public QuickDescriptionParser(OWLServer kit){
//        this(kit, kit.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(ServerConstants.RELATED_TO));
//    }
//
//    public QuickDescriptionParser(OWLServer kit, OWLObjectProperty defaultProp) {
//        this.kit = kit;
//        this.defaultProp = defaultProp;
//    }
//
//    public OWLClassExpression parse(String str) throws ParseException {
//        List<OWLClass> classes = createClassList(str, false);
//        if (!classes.isEmpty()){
//            OWLClass root = classes.get(0);
//            classes.remove(0);
//
//            if (!classes.isEmpty()){
//                final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
//                final Set<OWLClassExpression> intersection = new HashSet<OWLClassExpression>();
//                intersection.add(root);
//                for (OWLClass cls : classes){
//                    // @@TODO select a property that has this class in the range if there is one
//                    OWLObjectProperty property = getPropertyForClass(root, cls);
//                    intersection.add(df.getOWLObjectSomeValuesFrom(property, cls));
//                }
//                return df.getOWLObjectIntersectionOf(intersection);
//            }
//            else{
//                return root;
//            }
//        }
//        return null;
//    }
//
//    // @@TODO should we make this independent of the reasoner??
//    // @@TODO should return multiple properties
//    private OWLObjectProperty getPropertyForClass(OWLClass root, OWLClass cls) {
//        OWLObjectProperty bestMatchProperty = defaultProp;
//        OWLClassExpression bestMatchRange = null;
//
//        try {
//            OWLReasoner r = kit.getOWLReasoner();
//            if (r.isSatisfiable(cls)){
//                for (OWLObjectProperty prop : getOWLObjectProperties()){
//                    if (isInRange(cls, prop)){
//                        final Set<OWLClassExpression> domains = prop.getDomains(kit.getOntologies());
//                        if (!domains.isEmpty()){
//                            OWLClassExpression domain = createDescriptionFromClassSet(domains);
//                            if (r.isEquivalentClass(domain, root)){
//                                return prop;
//                            }
//                            else if (r.isSubClassOf(root, domain)){
//                                if (bestMatchProperty.equals(defaultProp)){
//                                    bestMatchProperty = prop;
//                                    bestMatchRange = domain;
//                                }
//                                else{
//                                    // if the range is narrower than the previous best match
//                                    if (r.isSubClassOf(domain,  bestMatchRange)){
//                                        bestMatchProperty = prop;
//                                        bestMatchRange = domain;
//                                    }
//                                }
//                            }
//                        }
//                    }
//                }
//            }
//            else{
//                logger.debug("TO IMPLEMENT: get narrow property for unsatisfiable class: " + cls);
//            }
//        }
//        catch (OWLReasonerException e) {
//            logger.error("Caught Exception: ", e);
//        }
//        return bestMatchProperty;
//    }
//
//    private boolean isInRange(OWLClass filler, OWLObjectProperty prop) throws OWLReasonerException {
//        boolean isInRange = true;
//        OWLReasoner r = kit.getOWLReasoner();
//        final Set<OWLClassExpression> propRanges = prop.getRanges(kit.getOntologies());
//        if (!propRanges.isEmpty()){
//            OWLClassExpression propRange = createDescriptionFromClassSet(propRanges);
//            isInRange = r.isSubClassOf(filler, propRange);
//        }
//        return isInRange;
//    }
//
//    private OWLClassExpression createDescriptionFromClassSet(Set<OWLClassExpression> clses){
//        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
//
//        OWLClassExpression descr = null;
//        if (!clses.isEmpty()){
//            if (clses.size() == 1){
//                descr = clses.iterator().next();
//            }
//            else if (clses.size() > 1){
//                descr = df.getOWLObjectIntersectionOf(clses);
//            }
//        }
//        return descr;
//    }
//
//    private List<OWLClass> createClassList(String string, boolean createUnresolvedClasses) throws ParseException {
//        final List<OWLClass> classes = new ArrayList<OWLClass>();
//        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
//
//        String[] elements = string.split(" ");
//        for (String element : elements){
//            String name = element.trim();
//            Set<OWLClass> matches = kit.getFinder().getOWLClasses(name);
//            if (matches.isEmpty()){
//                if (createUnresolvedClasses){
//                    try {
//                        final OWLClass newClass = df.getOWLClass(new URI(kit.getActiveOntology().getURI().toString() + "#" + name));
//                        classes.add(newClass);
//                        final AddAxiom addAxiom = new AddAxiom(kit.getActiveOntology(), df.getOWLDeclarationAxiom(newClass));
//                        kit.getOWLOntologyManager().applyChange(addAxiom);
//                        logger.debug("Added new class: " + newClass);
//                    }
//                    catch (Exception e) {
//                        throw new ParseException(e.getMessage(), 0);
//                    }
//                }
//                else{
//                    classes.add(df.getOWLThing());
//                }
//            }
//            else if(matches.size() == 1){
//                classes.add(matches.iterator().next());
//            }
//            else{
//                throw new ParseException("Ambiguous phrase: " + name + " resolves to multiple classes", 0);
//            }
//        }
//        return classes;
//    }
//
//    private Set<OWLObjectProperty> getOWLObjectProperties() {
//        Set<OWLObjectProperty> properties = new HashSet<OWLObjectProperty>();
//        for (OWLOntology ont : kit.getOntologies()){
//            properties.addAll(ont.getReferencedObjectProperties());
//        }
//        return properties;
//    }
}

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
 * Date: Aug 24, 2007<br><br>
 *
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

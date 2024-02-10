package org.coode.www.renderer;

import org.junit.Test;

import java.io.IOException;
import java.net.URL;
import java.util.Scanner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class HighlighterTest {

    @Test
    public void shouldReturnOriginalTextWhenNoMatch() {
        String highlight = "some (hadRole some StormTrooper)";

        String source = """
            Infiltrating_the_First_Order: included some (
            Injury
            and (of some (hadRole some Trooper)))""";


        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertEquals(source, result);
    }

    @Test
    public void shouldHighlightMatchingText() {
        String highlight = "some (hadRole some StormTrooper)";

        String source = """
            Infiltrating_the_First_Order: included some (
            Injury
            and (of some (hadRole some StormTrooper)))""";

        String expected = """
            Infiltrating_the_First_Order: included some (
            Injury
            and (of <span class="highlight">some (hadRole some StormTrooper)</span>))""";

        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertEquals(expected, result);
    }

    @Test
    public void shouldHighlightAllOccurrancesOfMatchingText() {
        String highlight = "some (hadRole some StormTrooper)";

        String source = """
            Infiltrating_the_First_Order: included some (
            (Injury and (of some (hadRole some StormTrooper))) and
            (Death and (of some (hadRole some StormTrooper)))
            )""";

        String expected = """
            Infiltrating_the_First_Order: included some (
            (Injury and (of <span class="highlight">some (hadRole some StormTrooper)</span>)) and
            (Death and (of <span class="highlight">some (hadRole some StormTrooper)</span>))
            )""";

        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertEquals(expected, result);
    }


    @Test
    public void shouldReturnEmptyWhenEmpty() {
        String result = new Highlighter("abc").highlight("");
        assertEquals("", result);
    }

    @Test
    public void shouldReturnOriginalMarkupWhenNoMatch() {
        String result = new Highlighter("abc").highlight("<a>defg");
        assertEquals("<a>defg", result);
    }

    @Test
    public void shouldHighlightMarkupStartingWithTag() {
        String result = new Highlighter("defg").highlight("<a>abcdefgh");
        assertEquals("<a>abc<span class=\"highlight\">defg</span>h", result);
    }

    @Test
    public void shouldHighlightMarkupEndingWithTag() {
        String result = new Highlighter("defg").highlight("abcdefg<a>");
        assertEquals("abc<span class=\"highlight\">defg</span><a>", result);
    }

    @Test
    public void shouldHighlightMarkupStartingAndEndingWithTag() {
        String result = new Highlighter("defg").highlight("<a>abcdefg</a>");
        assertEquals("<a>abc<span class=\"highlight\">defg</span></a>", result);
    }

    @Test
    public void shouldHighlightMarkupSplitOverTags() {
        String result = new Highlighter("defg").highlight("<a>abcde<em>fg</em></a>");
        assertEquals("<a>abc<span class=\"highlight\">de</span><em><span class=\"highlight\">fg</span></em></a>", result);
    }

    @Test
    public void shouldHighlightMarkupSplitOverTagsWithExtraContent() {
        String result = new Highlighter("defg").highlight("<a>abcde<em>fghij</em></a>");
        assertEquals("<a>abc<span class=\"highlight\">de</span><em><span class=\"highlight\">fg</span>hij</em></a>", result);
    }

    @Test
    public void shouldHighlightMarkupSplitOverManyTagsWithExtraContent() {
        String result = new Highlighter("cdefghi").highlight("<a>abc<em>def</em>ghi</a>");
        assertEquals("<a>ab<span class=\"highlight\">c</span><em><span class=\"highlight\">def</span></em><span class=\"highlight\">ghi</span></a>", result);
    }

    @Test
    public void shouldHighlightMarkupIgnoringOtherNonMatchingText() {
        String result = new Highlighter("cdefghi").highlight("<a>preamble</a>and<a>abc<em>def</em>ghi</a>");
        assertEquals("<a>preamble</a>and<a>ab<span class=\"highlight\">c</span><em><span class=\"highlight\">def</span></em><span class=\"highlight\">ghi</span></a>", result);
    }

    @Test
    public void shouldHighlightMarkupWithMutipleMatches() {
        String result = new Highlighter("abc").highlight("<a>preamble</a>a<a>bc<em>def</em>ghiabc</a>");
        assertEquals("<a>preamble</a><span class=\"highlight\">a</span><a><span class=\"highlight\">bc</span><em>def</em>ghi<span class=\"highlight\">abc</span></a>", result);
    }

    @Test
    public void shouldHighlightMatchingTextIgnoringMarkup() {
        // Infiltrating_the_First_Order: included some (
        //    Injury
        //    and (of some (hadRole some StormTrooper)))
        String highlight = "some (hadRole some StormTrooper)";
        String source = "<a href=\"/individuals/-1399458952/\" class=\"Individual\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Infiltrating_the_First_Order\">Infiltrating_<wbr>the_<wbr>First_<wbr>Order</a>: <a href=\"/objectproperties/1035051157/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#included\">included</a> <span class=\"some\">some</span> (<br>&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/classes/-1155386512/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Injury\">Injury</a><br>&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"keyword\">and</span> (<a href=\"/objectproperties/944795056/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#of\">of</a> <span class=\"some\">some</span> (<a href=\"/objectproperties/1627826554/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#hadRole\">hadRole</a> <span class=\"some\">some</span> <a href=\"/classes/-2145398193/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#StormTrooper\">StormTrooper</a>)))";
        String expected = "<a href=\"/individuals/-1399458952/\" class=\"Individual\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Infiltrating_the_First_Order\">Infiltrating_<wbr>the_<wbr>First_<wbr>Order</a>: <a href=\"/objectproperties/1035051157/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#included\">included</a> <span class=\"some\">some</span> (<br>&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/classes/-1155386512/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Injury\">Injury</a><br>&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"keyword\">and</span> (<a href=\"/objectproperties/944795056/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#of\">of</a> <span class=\"some\"><span class=\"highlight\">some</span></span><span class=\"highlight\"> (</span><a href=\"/objectproperties/1627826554/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#hadRole\"><span class=\"highlight\">hadRole</span></a><span class=\"highlight\"> </span><span class=\"some\"><span class=\"highlight\">some</span></span><span class=\"highlight\"> </span><a href=\"/classes/-2145398193/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#StormTrooper\"><span class=\"highlight\">StormTrooper</span></a><span class=\"highlight\">)</span>))";

        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertEquals(expected, result);
    }


    @Test
    public void shouldHighlightIgnoringCase() {
        // Infiltrating_the_First_Order: included some (
        //    Injury
        //    and (of some (hadRole some StormTrooper)))
        String highlight = "some (hadrole some stormtrooper)";
        String source = "<a href=\"/individuals/-1399458952/\" class=\"Individual\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Infiltrating_the_First_Order\">Infiltrating_<wbr>the_<wbr>First_<wbr>Order</a>: <a href=\"/objectproperties/1035051157/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#included\">included</a> <span class=\"some\">some</span> (<br>&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/classes/-1155386512/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Injury\">Injury</a><br>&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"keyword\">and</span> (<a href=\"/objectproperties/944795056/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#of\">of</a> <span class=\"some\">some</span> (<a href=\"/objectproperties/1627826554/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#hadRole\">hadRole</a> <span class=\"some\">some</span> <a href=\"/classes/-2145398193/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#StormTrooper\">StormTrooper</a>)))";
        String expected = "<a href=\"/individuals/-1399458952/\" class=\"Individual\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Infiltrating_the_First_Order\">Infiltrating_<wbr>the_<wbr>First_<wbr>Order</a>: <a href=\"/objectproperties/1035051157/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#included\">included</a> <span class=\"some\">some</span> (<br>&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/classes/-1155386512/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#Injury\">Injury</a><br>&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"keyword\">and</span> (<a href=\"/objectproperties/944795056/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#of\">of</a> <span class=\"some\"><span class=\"highlight\">some</span></span><span class=\"highlight\"> (</span><a href=\"/objectproperties/1627826554/\" class=\"Object Property\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#hadRole\"><span class=\"highlight\">hadRole</span></a><span class=\"highlight\"> </span><span class=\"some\"><span class=\"highlight\">some</span></span><span class=\"highlight\"> </span><a href=\"/classes/-2145398193/\" class=\"Class\" title=\"https://nickdrummond.github.io/star-wars-ontology/ontologies#StormTrooper\"><span class=\"highlight\">StormTrooper</span></a><span class=\"highlight\">)</span>))";

        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertEquals(expected, result);
    }

    @Test
    public void shouldHighlightLongMarkup() throws IOException {
        String highlight = "offa";
        URL testContent = getClass().getClassLoader().getResource("highlight-test.txt");
        Scanner s = new Scanner(testContent.openStream()).useDelimiter("\\A");
        String source = s.hasNext() ? s.next() : "";
        Highlighter h = new Highlighter(highlight);
        String result = h.highlight(source);

        assertTrue(result.contains("<span class=\"highlight\">offa</span>"));
    }
}

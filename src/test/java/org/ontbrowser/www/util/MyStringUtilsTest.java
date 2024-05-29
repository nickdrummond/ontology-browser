package org.ontbrowser.www.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class MyStringUtilsTest {

    @Test
    public void sanitizeForRegex() {
        assertEquals("\\[ \\* \\+ \\? \\{ \\. \\( \\) \\^ \\$ \\| \\ \\] \\-", MyStringUtils.sanitiseForRegex("[ * + ? { . ( ) ^ $ | \\ ] -"));
    }
}
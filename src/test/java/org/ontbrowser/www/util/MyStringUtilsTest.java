package org.ontbrowser.www.util;


import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class MyStringUtilsTest {

    @Test
    public void sanitizeForRegex() {
        assertEquals("\\[ \\* \\+ \\? \\{ \\. \\( \\) \\^ \\$ \\| \\ \\] \\-", MyStringUtils.sanitiseForRegex("[ * + ? { . ( ) ^ $ | \\ ] -"));
    }
}
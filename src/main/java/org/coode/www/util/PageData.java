package org.coode.www.util;

public record PageData(
        int start,
        int pageSize,
        int total
){}
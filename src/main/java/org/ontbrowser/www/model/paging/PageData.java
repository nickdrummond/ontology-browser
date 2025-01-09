package org.ontbrowser.www.model.paging;

public record PageData(
        int start,
        int pageSize,
        int total
){}
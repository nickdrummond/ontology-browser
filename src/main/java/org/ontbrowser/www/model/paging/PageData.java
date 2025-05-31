package org.ontbrowser.www.model.paging;

public record PageData(
        int start,
        int pageSize,
        int total
){

    public PageData(int start, int pageSize) {
        this(start, pageSize, Integer.MAX_VALUE);
    }

    boolean hasTotal() {
        return total != Integer.MAX_VALUE;
    }
}
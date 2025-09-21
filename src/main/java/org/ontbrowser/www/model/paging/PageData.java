package org.ontbrowser.www.model.paging;

public record PageData(
        int start,
        int pageSize,
        int total
){

    public PageData(int start, int pageSize) {
        this(start, pageSize, Integer.MAX_VALUE);
    }

    // Methods used in templates

    public int end() {
        return Math.min(total, start + pageSize-1);
    }

    public boolean hasTotal() {
        return total != Integer.MAX_VALUE;
    }

    public boolean isSinglePage() {
        return (start == 1) && (pageSize >= total);
    }

    public boolean hasPrevious() {
        return start > 1;
    }

    public PageData getPrevious() {
        return new PageData(Math.max(start - pageSize, 1), pageSize, total);
    }

    public boolean hasNext() {
        return (start + pageSize) < total;
    }

    public PageData getNext() {
        return new PageData(start + pageSize, pageSize, total);
    }
}
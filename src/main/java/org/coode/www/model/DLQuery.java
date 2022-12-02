package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLClassExpression;

import javax.annotation.Nonnull;
import java.util.Objects;

public class DLQuery {

    private final OWLClassExpression owlClassExpression;

    private final QueryType queryType;

    public DLQuery(@Nonnull OWLClassExpression owlClassExpression, @Nonnull QueryType queryType) {
        this.owlClassExpression = owlClassExpression;
        this.queryType = queryType;
    }

    public OWLClassExpression getOwlClassExpression() {
        return owlClassExpression;
    }

    public QueryType getQueryType() {
        return queryType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DLQuery query = (DLQuery) o;
        return owlClassExpression.equals(query.owlClassExpression) && queryType == query.queryType;
    }

    @Override
    public int hashCode() {
        return Objects.hash(owlClassExpression, queryType);
    }
}

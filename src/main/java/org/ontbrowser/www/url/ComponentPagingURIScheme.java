package org.ontbrowser.www.url;

import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.model.paging.WithEditor;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;
import java.util.List;
import java.util.stream.Stream;

import static org.springframework.web.util.UriComponentsBuilder.fromUri;

public class ComponentPagingURIScheme extends PagingURIScheme {

    private final List<With> with;

    public ComponentPagingURIScheme(HttpServletRequest request, List<With> with) {
        super(request);
        this.with = with == null ? List.of() : with;
    }

    public URI updateForPage(String componentName, int start, int pageSize) {
        Stream<With> updatedWith = isNoneMatch(componentName)
                ? addParam(componentName, start, pageSize)
                : updateMatchingParam(componentName, start, pageSize);

        return fromUri(original)
                .replaceQueryParam("with", updatedWith.map(WithEditor::asText).toList())
                .build().toUri();
    }

    private Stream<With> updateMatchingParam(String componentName, int start, int pageSize) {
        return with.stream().map(w -> w.characteristicName().equals(componentName)
                ? new With(componentName, start, pageSize)
                : w);
    }

    private Stream<With> addParam(String componentName, int start, int pageSize) {
        return Stream.concat(Stream.of(new With(componentName, start, pageSize)), with.stream());
    }

    private boolean isNoneMatch(String componentName) {
        return with.stream().noneMatch(w -> w.characteristicName().equals(componentName));
    }
}

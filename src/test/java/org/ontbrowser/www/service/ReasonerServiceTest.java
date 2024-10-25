package org.ontbrowser.www.service;

import org.apache.commons.collections4.map.LRUMap;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.feature.dlquery.DLQuery;
import org.ontbrowser.www.feature.dlquery.QueryType;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import uk.ac.manchester.cs.owl.owlapi.OWLClassImpl;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import static org.junit.Assert.*;

@RunWith(MockitoJUnitRunner.class)
public class ReasonerServiceTest {

    public final DLQuery q1 = new DLQuery(new OWLClassImpl(IRI.create("test")), QueryType.instances);
    public final DLQuery q2 = new DLQuery(new OWLClassImpl(IRI.create("test2")), QueryType.instances);

    @Mock
    private OWLHTMLKit kit;

    @Mock
    private ReasonerFactoryService reasonerFactoryService;

    @Mock
    private ExecutorService reasonerThreadPool;

    @Test
    public void multipleRequestsForTheSameQueryResultInOneReasonerCall() {

        Map<DLQuery, Future<Set<OWLEntity>>> cache = Collections.synchronizedMap(new LRUMap<>(10));
        ReasonerService service = new ReasonerService(kit, reasonerThreadPool, cache, reasonerFactoryService);

        Future<Set<OWLEntity>> request = service.asyncQuery(q1);
        Future<Set<OWLEntity>> request2 = service.asyncQuery(q1);

        assertSame(request, request2);
        assertEquals(1, cache.size());
    }
}
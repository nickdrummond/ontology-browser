package org.coode.www.mngr;

import org.slf4j.LoggerFactory; import org.slf4j.Logger;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.OntServerException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;
import java.net.MalformedURLException;
import java.net.URL;


/**
 * For a given key we will have completely independent ontology managers running with seperate reasoners etc
 * as each will be handling different ontologies (each session is independent).
 *
 * can 2 sessions ever use the same reasoner if the ontology loaded is the same? Probably not as we don't
 * know this is the same version of the same ontology (as changes are not forbidden)
 */
public class SessionManager {

    private static Logger logger = LoggerFactory.getLogger(SessionManager.class);

    private static final String KIT_KEY = "kit";
    private static final String KIT_CLEANUP = "kitcleanup";

    private final KitRepository repo;

    public SessionManager(final KitRepository repo) {
        this.repo = repo;
    }

    /**
     * Get a server (creates a new one if you have a new session)
     * @param request
     * @return
     * @throws OntServerException
     */
    public OWLHTMLKit getHTMLKit(final HttpServletRequest request) throws OntServerException {
        HttpSession session = request.getSession(true);

        if (session.getAttribute(KIT_KEY) == null){
            create(session, request);
        }

        return (OWLHTMLKit)session.getAttribute(KIT_KEY);
    }


    /**
     * Copy a server that has been created/saved by another user - dumps all current state
     * @param request
     * @param label
     * @return
     * @throws OntServerException
     */
    public OWLHTMLKit getHTMLKit(final HttpServletRequest request, final String label) throws OntServerException {
        OWLHTMLKit kit = getHTMLKit(request);
        if (label != null && !label.equals(kit.getCurrentLabel())){
            repo.loadKit(kit, label);
        }
        return kit;
    }

    public void closeSession(final HttpServletRequest request) {
        HttpSession mySession = request.getSession(false);
        if (mySession != null){
            mySession.invalidate();
        }
    }

    private void create(final HttpSession mySession, final HttpServletRequest request) {
        try{
            logger.debug("Creating a new Session: " + mySession.getId());

            // TODO Why do we need the base URL in the kit?
            String url = request.getRequestURL().toString();

            String servletPath = request.getServletPath();

            if (!servletPath.equals("/")) {
                int index = url.indexOf(servletPath);

                if (index != -1){
                    url = url.substring(0, index+1);
                }
            }

            URL basePath = new URL(url);

            OWLHTMLKit kit = repo.createHTMLKit(basePath);
            KitCleanupAdapter adapter = new KitCleanupAdapter(kit);

            mySession.setAttribute(KIT_KEY, kit);
            mySession.setAttribute(KIT_CLEANUP, adapter);
        }
        catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * This adapter will get notified when a session expires
     * so that it can perform cleanup.
     */
    class KitCleanupAdapter implements HttpSessionBindingListener {
        private final OWLHTMLKit kit;

        public KitCleanupAdapter(final OWLHTMLKit kit) {
            this.kit = kit;
        }

        public OWLHTMLKit getKit() {
            return kit;
        }

        public void valueBound(final HttpSessionBindingEvent event) {
            logger.debug("Added Kit to session " + event.getSession().getId());
        }

        public void valueUnbound(final HttpSessionBindingEvent event) {
            logger.debug("Disposing of Kit for session " + event.getSession().getId());
            kit.dispose();
            System.gc();
        }
    }
}

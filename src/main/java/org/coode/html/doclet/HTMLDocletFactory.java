package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 22, 2010<br><br>
 */
@Deprecated
public class HTMLDocletFactory {

    private Map<String, Class<? extends HTMLDoclet>> allDoclets = new HashMap<String, Class<? extends HTMLDoclet>>();

    private OWLHTMLKit kit;

    public HTMLDocletFactory(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @SuppressWarnings("unchecked")
    public HTMLDoclet getDoclet(String id){
        try {
            Class<? extends HTMLDoclet> cls = allDoclets.get(id);
            if (cls == null){
                return null;
            }
            Constructor<? extends HTMLDoclet> constructor = cls.getConstructor(OWLHTMLKit.class);
            return constructor.newInstance(kit);
        }
        catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
        catch (InvocationTargetException e) {
            e.printStackTrace();
        }
        catch (InstantiationException e) {
            e.printStackTrace();
        }
        catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }

    public Set<String> getAvailableDoclets(){
        return allDoclets.keySet();
    }

    public void register(String id, Class<? extends HTMLDoclet> cls){
        try {
            cls.getConstructor(OWLHTMLKit.class);
            allDoclets.put(id, cls);
        }
        catch (NoSuchMethodException e) {
            throw new RuntimeException("Cannot register doclet " + id + " as it has not got the correct constructor");
        }
    }

    public void load(Properties properties) throws IOException {
        for (String name : properties.stringPropertyNames()) {
            try {
                final Class cls = Class.forName(properties.getProperty(name).trim());
                final Class<? extends HTMLDoclet> impl = cls.asSubclass(HTMLDoclet.class);
                register(name, impl);
            } catch (ClassNotFoundException e) {
                System.err.println("Malformed doclet descriptor (cannot find class): " +  name);
            } catch (ClassCastException e) {
                System.err.println("Malformed doclet descriptor (class does not implement HTMLDoclet): " + name);
            }
        }
    }
}

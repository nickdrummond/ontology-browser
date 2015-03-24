package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;

import java.io.BufferedReader;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 22, 2010<br><br>
 */
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

    public void load(BufferedReader reader) throws IOException {
        String line;
        while((line = reader.readLine()) != null){
            line = line.trim();
            if (line.length() == 0 || line.startsWith("//")){
                // do nothing
            }
            else{
                String[] args = line.split(",");
                if (args.length == 2){
                    try {
                        final Class cls = Class.forName(args[1].trim());
                        final Class<? extends HTMLDoclet> impl = cls.asSubclass(HTMLDoclet.class);
                        register(args[0].trim(), impl);
                    }
                    catch (ClassNotFoundException e) {
                        System.err.println("Malformed doclet descriptor (cannot find class): " + line);
                    }
                    catch (ClassCastException e){
                        System.err.println("Malformed doclet descriptor (class does not implement HTMLDoclet): " + line);
                    }
                }
                else{
                    System.err.println("Malformed doclet descriptor: " + line);
                }
            }
        }
    }
}

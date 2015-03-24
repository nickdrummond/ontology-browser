package org.coode.www;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.www.exception.InvalidRequestException;
import org.coode.www.servlet.AbstractOntologyServerServlet;

import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 20, 2010<br><br>
 */
public class ParametersBuilder {

    public Map<OWLHTMLParam, String> checkAndCreateParams(HttpServletRequest request,
                                                           OWLHTMLKit kit,
                                                           AbstractOntologyServerServlet servlet) throws InvalidRequestException {

        Map<OWLHTMLParam, String> params = new HashMap<OWLHTMLParam, String>();

        final Map<OWLHTMLParam, Set<String>> requiredParams = servlet.getRequiredParams(kit.getOWLServer());

        // check the parameters are known
        for (Object key: request.getParameterMap().keySet()){
            try{
                params.put(OWLHTMLParam.valueOf((String)key), null);
            }
            catch (IllegalArgumentException e){
                throw new InvalidRequestException(request, requiredParams);
            }
        }

        // check that the required parameters are given
        for (OWLHTMLParam requiredParam : requiredParams.keySet()){
            if (!params.keySet().contains(requiredParam)){
                throw new InvalidRequestException(request, requiredParams);
            }
        }


        for (OWLHTMLParam param : params.keySet()){
            String[] values = (String[])request.getParameterMap().get(param.name());
            String value = values[0].trim();
            switch(param){
                case session: break;
                case format: break;
                case uri:    // eg people+pets.owl gets corrupted otherwise
                case parent: // dropthrough
                    try {
                        // hack to handle spaces (as the HttpServletRequest seems to decode these)
                        value = value.replace(' ', '+');
                        // hack to ensure that params are decoded (if not already uri escaped)
                        if (request.getCharacterEncoding() == null && !value.startsWith("%")){
                            value = new String(value.getBytes("8859_1"), OWLHTMLConstants.DEFAULT_ENCODING);
                        }
                        params.put(param, value);
                    }
                    catch (UnsupportedEncodingException e) {
                        throw new RuntimeException(e);
                    }
                    break;
                default:
                    try {
                        // hack to ensure that params are decoded (if not already uri escaped)
                        if (request.getCharacterEncoding() == null && !value.startsWith("%")){
                            value = new String(value.getBytes("8859_1"), OWLHTMLConstants.DEFAULT_ENCODING);
                        }
                        value = URLDecoder.decode(value, OWLHTMLConstants.DEFAULT_ENCODING);
                        params.put(param, value);
                    }
                    catch (UnsupportedEncodingException e) {
                        throw new RuntimeException(e);
                    }
            }
        }
        return params;
    }
}

package org.coode.html.util;

import java.io.*;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Mar 27, 2008<br><br>
 */
public class FileUtils {

    private String encoding;

    public FileUtils(String resourcesDir, String encoding) {
        this.encoding = encoding;
    }


    public  void copyFile(File in, File out) throws Exception {
        FileInputStream fis  = new FileInputStream(in);
        FileOutputStream fos = new FileOutputStream(out);
        byte[] buf = new byte[1024];
        int i = 0;
        while((i=fis.read(buf))!=-1) {
            fos.write(buf, 0, i);
        }
        fis.close();
        fos.close();
    }


    public  void saveFile(InputStream fis, File file) throws IOException {
        OutputStream fos = new FileOutputStream(file);
        byte[] buffer = new byte[1024];
        int l;
        while((l = fis.read(buffer)) > 0){
            fos.write(buffer, 0, l);
        }
        fis.close();
        fos.close();
    }


    public  PrintWriter open(File file) throws IOException {
        return new PrintWriter(getFileWriter(file));
    }


    private Writer getFileWriter(File file) throws IOException{
        OutputStream fos = new FileOutputStream(file);
        OutputStream buffer= new BufferedOutputStream(fos);
        return new OutputStreamWriter(buffer, encoding);
    }
}

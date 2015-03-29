<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>
<spring:url var="ontPostUrl" value="/ontologies"/>

<div id='load_ontologies'>
<h4>Load Ontologies</h4>
<div class='codebox'>
    <form id='bookmarks' method='POST' action='${ontPostUrl}' target='_top' >
        <label for='uri-bookmark'><h3 style='margin-bottom: 0;'>or Select a bookmark from below:</h3></label><br />
        <select id='uri-bookmark' name='uri' style='width:80%; margin-top: 0;'>
        </select>
        <input name='action' type='submit' value='load' />
    </form>
    <form id='load' method='POST' action='${ontPostUrl}' target='_top' >
        <label for='uri-spec'><h3 style='margin-bottom: 0;'>Specify the physical location of your ontology:</h3></label><br />
        <input id='uri-spec' name='uri' type='text' style='width:80%; margin-top: 0;' />
        <input name='action' type='submit' value='load' />
    </form>
    <!--form method='post' enctype='multipart/form-data' action='.'>
      <p class='instructions'>Upload an ontology:</p>
      <p><label title='Choose a Local File to Upload and Validate' for='uploaded_file'>File:</label>
        <input type='file' id='uploaded_file' name='uploaded_file' size='30' /></p>
        <input name='action' type='submit' value='load' />
    </form-->
<br style='clear: both;' />
</div>
</div><!-- load ontologies -->
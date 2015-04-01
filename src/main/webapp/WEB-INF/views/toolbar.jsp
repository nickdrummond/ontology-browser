<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>

<img class='icon' src='<spring:url value="/static/images/splat-24-211412.png"/>'/>
<h1><a href='<spring:url value="/"/>'>${application.name}</a></h1>

<div id='menu'>

    <a style='display: none;' href='#content'>skip to content</a>

    <form id="activeOnt">
        <a id='signout' href='<spring:url value="/signout"/>' target='_top'>
            <img src='<spring:url value="/static/images/close.png"/>' width='16' height='16' title='close' />
        </a>
        <input type='hidden' name='property' value='optionActiveOnt' />
        <select class="option" name="value">
            <c:forEach items="${ontologies}" var="ontology">
                <option value='${ontology.ontologyID.ontologyIRI}'
                 <c:if test="${ontology.ontologyID.ontologyIRI eq activeOntology.ontologyID.ontologyIRI}">
                   selected='selected'
                 </c:if>
                 >${ontology.ontologyID.ontologyIRI}</option>
            </c:forEach>
        </select>
    </form>

    <form class='autocomplete' method='get' id='findForm' action='<spring:url value="/find/"/>'>
        <input type='text' name='input'  id='find'/>
        <input name='uri' type='hidden' value='' />
        <input name='syntax' id='dlQuerySyntax' type='hidden' value='man' />
        <input type='submit' value='find' />
    </form>
    <script type='text/javascript'>
        var options = {
            script: '<spring:url value="/find/?format=xml&type=entities&"/>',
            varname: 'input',
            cache: false,
            callback: function (obj){
                $('#findForm input[name=uri]').val(obj.id);
                $('#findForm').submit();
            }
        };
        var as = new AutoSuggest("find", options);
    </script>

    <div id='options'>
        <a class='optionLink' href='<spring:url value="/options/"/>' >Options</a>
        <form id='rendererForm' style='display: inline;'>
            <label for='renderLabels'>Render labels</label>
            <input type='checkbox' name='renderLabels' id="renderLabels" checked='checked' />
        </form>
    </div> <!-- options -->

</div> <!-- menu -->


<div id='tabs'>
    <a href='<spring:url value="/ontologies/"/>'>Ontologies</a>
    <a href='<spring:url value="/classes/"/>'>Classes</a>
    <a href='<spring:url value="/individuals/"/>'>Individuals</a>
    <a href='<spring:url value="/objectproperties/"/>'>Object Properties</a>
    <a href='<spring:url value="/dataproperties/"/>'>Data Properties</a>
    <a href='<spring:url value="/annotationproperties/"/>'>Annotation Properties</a>
    <a href='<spring:url value="/datatypes/"/>'>Datatypes</a>
</div> <!-- tabs -->

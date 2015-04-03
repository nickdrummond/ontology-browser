<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>

<img class='icon' src='<spring:url value="/static/images/splat-24-211412.png"/>'/>
<h1><a href='<spring:url value="/"/>'>${application.name}</a></h1>

        <a id='signout' href='<spring:url value="/signout"/>' target='_top'>
            <img src='<spring:url value="/static/images/close.png"/>' width='16' height='16' title='close' />
        </a>

<div id='menu'>

    <a style='display: none;' href='#content'>skip to content</a>
    <form id="activeOnt">
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

    <form class='autocomplete' method='get' id='findForm'>
        <input type='text' name='input' placeholder='Search for an Entity' id='find'/>
    </form>
    <script type='text/javascript'>
        var options = {
            script: '<spring:url value="/entities/?"/>',
            varname: 'name',
            cache: false,
            callback: function (obj){
                window.location = obj.id;
            }
        };
        var as = new AutoSuggest("find", options);
    </script>

    <div id='options'>
        <a class='optionLink' href='<spring:url value="/options/"/>' >Options</a>
        <form id='rendererForm'>
            <label for='renderLabels'>Labels</label>
            <input type='checkbox' name='renderLabels' id="renderLabels" checked='checked' />
        </form>
    </div> <!-- options -->

    <div class='clearfix'></div>

</div> <!-- menu -->


<div id='tabs'>
    <a href='<spring:url value="/ontologies/"/>'>Ontologies</a>
    <a href='<spring:url value="/classes/"/>'>Classes</a>
    <a href='<spring:url value="/individuals/"/>'>Individuals</a>
    <a href='<spring:url value="/objectproperties/"/>'>Object&nbsp;Properties</a>
    <a href='<spring:url value="/dataproperties/"/>'>Data&nbsp;Properties</a>
    <a href='<spring:url value="/annotationproperties/"/>'>Annotation&nbsp;Properties</a>
    <a href='<spring:url value="/datatypes/"/>'>Datatypes</a>
</div> <!-- tabs -->

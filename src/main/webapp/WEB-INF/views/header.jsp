<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>

<head>
<title>${applicationName}</title>

    <meta http-equiv='content-type' content='text/html;charset=UTF-8'>

    <link rel='stylesheet' href='<spring:url value="/static/css/autosuggest_inquisitor.css"/>' type='text/css' />
    <link rel='stylesheet' href='<spring:url value="/static/css/default.css"/>' type='text/css' />

    <script src='<spring:url value="/static/js/bsn.AutoSuggest_2.1_multiword.js"/>' type='text/javascript'></script>
    <script src='<spring:url value="/static/js/jquery-1.4.3.min.js"/>' type='text/javascript'></script>
    <script src='<spring:url value="/static/js/default.js"/>' type='text/javascript'></script>
    <script type='text/javascript'>
        baseURL="<spring:url value="/"/>";
        document.getElementById("uri-spec").focus();
    </script>

</head>
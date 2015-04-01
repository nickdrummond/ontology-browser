<%@taglib uri="http://www.springframework.org/tags" prefix="spring"%>

<head>
<title>${application.name}</title>

    <meta http-equiv='content-type' content='text/html;charset=UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=3.0'>

    <link rel='icon'       type='image/x-icon' href='<spring:url value="/static/images/favicon.ico"/>'/>
    <link rel='apple-touch-icon-precomposed'   href='<spring:url value="/static/images/splat-152-211412.png"/>'/>

    <link rel='stylesheet' type='text/css'     href='<spring:url value="/static/css/autosuggest_inquisitor.css"/>'/>
    <link rel='stylesheet' type='text/css'     href='<spring:url value="/static/css/default.css"/>'/>

    <script type='text/javascript' src='<spring:url value="/static/js/bsn.AutoSuggest_2.1_multiword.js"/>'></script>
    <script type='text/javascript' src='<spring:url value="/static/js/jquery-1.4.3.min.js"/>'></script>
    <script type='text/javascript' src='<spring:url value="/static/js/default.js"/>'></script>
    <script type='text/javascript'>
        baseURL="<spring:url value="/"/>";
        document.getElementById("uri-spec").focus();
    </script>

</head>
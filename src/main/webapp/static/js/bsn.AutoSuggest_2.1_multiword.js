/**
 *  author:		Timothy Groves - http://www.brandspankingnew.net
 *	version:	1.2 - 2006-11-17
 *              1.3 - 2006-12-04
 *              2.0 - 2007-02-07
 *
 */

var useBSNns;

if (useBSNns)
{
    if (typeof(bsn) == "undefined")
        bsn = {};
    _bsn = bsn;
}
else
{
    _bsn = this;
}

if (typeof(_bsn.Autosuggest) == "undefined")
    _bsn.Autosuggest = {};

_bsn.AutoSuggest = function (id, param)
{
    // no DOM - give up!
    //
    if (!document.getElementById)
        return false;




    // get field via DOM
    //
    this.fld = _bsn.DOM.gE(id);

    if (!this.fld)
        return false;




    // init variables
    //
    this.sInput 		= "";
    this.nInputChars 	= 0;
    this.aSuggestions 	= [];
    this.iHighlighted 	= 0;




    // parameters object
    //
    this.oP = (param) ? param : {};

    // defaults
    //
    var def = {minchars:1, meth:"get", varname:"input", className:"autosuggest", timeout:2500, delay:500, offsety:-5, shownoresults: true, noresults: "No results!", maxheight: 250, cache: true, multiword: false};
    for (k in def)
        if (typeof(this.oP[k]) != typeof(def[k]))	this.oP[k] = def[k];



        // set keyup handler for field
        // and prevent autocomplete from client
        //
    var p = this;

    // NOTE: not using addEventListener because UpArrow fired twice in Safari
    //_bsn.DOM.addEvent( this.fld, 'keyup', function(ev){ return pointer.onKeyPress(ev); } );

    this.fld.onkeypress 	= function(ev){ return p.onKeyPress(ev); }
    this.fld.onkeyup 		= function(ev){ return p.onKeyUp(ev); }

    this.fld.setAttribute("autocomplete","off");
}
















_bsn.AutoSuggest.prototype.onKeyPress = function(ev)
{

    var key = (window.event) ? window.event.keyCode : ev.keyCode;



    // set responses to keydown events in the field
    // this allows the user to use the arrow keys to scroll through the results
    // ESCAPE clears the list
    // TAB sets the current highlighted value
    //
    var RETURN = 13;
    var TAB = 9;
    var ESC = 27;

    var bubble = true;

    switch(key)
            {
        case RETURN:
            bubble = this.setHighlightedValue();
            break;

        case ESC:
            this.clearSuggestions();
            break;
    }

    return bubble;
}



_bsn.AutoSuggest.prototype.onKeyUp = function(ev)
{
    var key = (window.event) ? window.event.keyCode : ev.keyCode;



    // set responses to keydown events in the field
    // this allows the user to use the arrow keys to scroll through the results
    // ESCAPE clears the list
    // TAB sets the current highlighted value
    //

    var ARRUP = 38;
    var ARRDN = 40;

    var bubble = true;

    switch(key)
            {


        case ARRUP:
            this.changeHighlight(key);
            bubble = false;
            break;


        case ARRDN:
            this.changeHighlight(key);
            bubble = false;
            break;


        default:
            if (this.oP.multiword){
                this.getSuggestions(this.getLastWord(this.fld.value));
            }
            else{
                this.getSuggestions(this.fld.value);
            }
    }

    return bubble;


}


_bsn.AutoSuggest.prototype.getSuggestions = function (val)
{

    // if input stays the same, do nothing
    //
    if (val == this.sInput)
        return false;


    // input length is less than the min required to trigger a request
    // reset input string
    // do nothing
    //
    if (val.length < this.oP.minchars)
    {
        this.sInput = "";
        return false;
    }


    // if caching enabled, and user is typing (ie. length of input is increasing)
    // filter results out of aSuggestions from last request
    //
    if (val.length>this.nInputChars && this.aSuggestions.length && this.oP.cache)
    {
        var arr = [];
        for (var i=0;i<this.aSuggestions.length;i++)
        {
            if (this.aSuggestions[i].value.substr(0,val.length).toLowerCase() == val.toLowerCase())
                arr.push( this.aSuggestions[i] );
        }

        this.sInput = val;
        this.nInputChars = val.length;
        this.aSuggestions = arr;

        this.createList(this.aSuggestions);



        return false;
    }
    else
    // do new request
    //
    {
        this.sInput = val;
        this.nInputChars = val.length;


        var pointer = this;
        clearTimeout(this.ajID);
        this.ajID = setTimeout( function() { pointer.doAjaxRequest() }, this.oP.delay );
    }

    return false;
}





_bsn.AutoSuggest.prototype.doAjaxRequest = function ()
{

    var pointer = this;

    var value = this.fld.value;
    if (this.oP.multiword){
        value = this.getLastWord(value);
    }
    
    // create ajax request
    //
    if (typeof(this.oP.script) == "function")
        var url = this.oP.script(encodeURI(value));
    else
        var url = this.oP.script+this.oP.varname+"="+encodeURI(value);

    if (!url)
        return false;

    var meth = this.oP.meth;

    var onSuccessFunc = function (req) { pointer.setSuggestions(req) };
    var onErrorFunc = function (status) { alert("AJAX error: "+status); };

    var myAjax = new _bsn.Ajax();
    myAjax.makeRequest( url, meth, onSuccessFunc, onErrorFunc );
}





_bsn.AutoSuggest.prototype.setSuggestions = function (req)
{
    this.aSuggestions = [];

    if (this.oP.json)
    {
        var jsondata = eval('(' + req.responseText + ')');

        for (var i=0;i<jsondata.results.length;i++)
        {
            this.aSuggestions.push(  { 'id':jsondata.results[i].id, 'value':jsondata.results[i].value, 'info':jsondata.results[i].info }  );
        }
    }
    else
    {

        var xml = req.responseXML;


        // traverse xml
        //
        var results = xml.getElementsByTagName('results')[0].childNodes;

        for (var i=0;i<results.length;i++)
        {
            if (results[i].hasChildNodes())
                this.aSuggestions.push(  { 'id':results[i].getAttribute('id'), 'value':results[i].childNodes[0].nodeValue, 'info':results[i].getAttribute('info') }  );
        }

    }

    this.idAs = "as_"+this.fld.id;


    this.createList(this.aSuggestions);

}














_bsn.AutoSuggest.prototype.createList = function(arr)
{
    var pointer = this;




    // get rid of old list
    // and clear the list removal timeout
    //
    _bsn.DOM.remE(this.idAs);
    this.killTimeout();


    // if no results, and shownoresults is false, do nothing
    //
    if (arr.length == 0 && !this.oP.shownoresults)
        return false;


    // create holding div
    //
    var div = _bsn.DOM.cE("div", {id:this.idAs, className:this.oP.className});

    var hcorner = _bsn.DOM.cE("div", {className:"as_corner"});
    var hbar = _bsn.DOM.cE("div", {className:"as_bar"});
    var header = _bsn.DOM.cE("div", {className:"as_header"});
    header.appendChild(hcorner);
    header.appendChild(hbar);
    div.appendChild(header);




    // create and populate ul
    //
    var ul = _bsn.DOM.cE("ul", {id:"as_ul"});




    // loop throught arr of suggestions
    // creating an LI element for each suggestion
    //
    for (var i=0;i<arr.length;i++)
    {
        // format output with the input enclosed in a EM element
        // (as HTML, not DOM)
        //
        var val = arr[i].value;
        var st = val.toLowerCase().indexOf( this.sInput.toLowerCase() );
        var output = val.substring(0,st) + "<em>" + val.substring(st, st+this.sInput.length) + "</em>" + val.substring(st+this.sInput.length);


        var span 		= _bsn.DOM.cE("span", {}, output, true);
        if (arr[i].info != "")
        {
            var br			= _bsn.DOM.cE("br", {});
            span.appendChild(br);
            var small		= _bsn.DOM.cE("small", {}, arr[i].info);
            span.appendChild(small);
        }

        var a 			= _bsn.DOM.cE("a", { href:"#" });

        var tl 		= _bsn.DOM.cE("span", {className:"tl"}, " ");
        var tr 		= _bsn.DOM.cE("span", {className:"tr"}, " ");
        a.appendChild(tl);
        a.appendChild(tr);

        a.appendChild(span);

        a.name = i+1;
        a.onclick = function () { pointer.setHighlightedValue(); return false; }
        a.onmouseover = function () { pointer.setHighlight(this.name); }

        var li 			= _bsn.DOM.cE(  "li", {}, a  );

        ul.appendChild( li );
    }


    // no results
    //
    if (arr.length == 0 && this.oP.shownoresults)
    {
        var li 			= _bsn.DOM.cE(  "li", {className:"as_warning"}, this.oP.noresults  );
        ul.appendChild( li );
    }


    div.appendChild( ul );


    var fcorner = _bsn.DOM.cE("div", {className:"as_corner"});
    var fbar = _bsn.DOM.cE("div", {className:"as_bar"});
    var footer = _bsn.DOM.cE("div", {className:"as_footer"});
    footer.appendChild(fcorner);
    footer.appendChild(fbar);
    div.appendChild(footer);



    // get position of target textfield
    // position holding div below it
    // set width of holding div to width of field
    //
    var pos = _bsn.DOM.getPos(this.fld);

    div.style.left 		= pos.x + "px";
    div.style.top 		= ( pos.y + this.fld.offsetHeight + this.oP.offsety ) + "px";
    div.style.width 	= this.fld.offsetWidth + "px";



    // set mouseover functions for div
    // when mouse pointer leaves div, set a timeout to remove the list after an interval
    // when mouse enters div, kill the timeout so the list won't be removed
    //
    div.onmouseover 	= function(){ pointer.killTimeout() }
    div.onmouseout 		= function(){ pointer.resetTimeout() }


    // add DIV to document
    //
    document.getElementsByTagName("body")[0].appendChild(div);



    // currently no item is highlighted
    //
    this.iHighlighted = 0;






    // remove list after an interval
    //
    var pointer = this;
    this.toID = setTimeout(function () { pointer.clearSuggestions() }, this.oP.timeout);
}















_bsn.AutoSuggest.prototype.changeHighlight = function(key)
{
    var list = _bsn.DOM.gE("as_ul");
    if (!list)
        return false;

    var n;

    if (key == 40)
        n = this.iHighlighted + 1;
    else if (key == 38)
        n = this.iHighlighted - 1;


    if (n > list.childNodes.length)
        n = list.childNodes.length;
    if (n < 1)
        n = 1;


    this.setHighlight(n);
}



_bsn.AutoSuggest.prototype.setHighlight = function(n)
{
    var list = _bsn.DOM.gE("as_ul");
    if (!list)
        return false;

    if (this.iHighlighted > 0)
        this.clearHighlight();

    this.iHighlighted = Number(n);

    var hNode = list.childNodes[this.iHighlighted-1];
    hNode.className = "as_highlight";
    hNode.scrollIntoView(false);

    this.killTimeout();
}


_bsn.AutoSuggest.prototype.clearHighlight = function()
{
    var list = _bsn.DOM.gE("as_ul");
    if (!list)
        return false;

    if (this.iHighlighted > 0)
    {
        list.childNodes[this.iHighlighted-1].className = "";
        this.iHighlighted = 0;
    }
}


_bsn.AutoSuggest.prototype.setHighlightedValue = function ()
{
    if (this.iHighlighted)
    {
        this.sInput = this.aSuggestions[ this.iHighlighted-1 ].value;

        if (this.oP.multiword){
            this.fld.value = this.setLastWord(this.sInput);
        }
        else{
            this.fld.value = this.sInput;
        }
        
        // move cursor to end of input (safari)
        //
        this.fld.focus();
        if (this.fld.selectionStart)
            this.fld.setSelectionRange(this.fld.value.length, this.fld.value.length);


        this.clearSuggestions();

        // pass selected object to callback function, if exists
        //
        if (typeof(this.oP.callback) == "function")
            this.oP.callback( this.aSuggestions[this.iHighlighted-1] );
        return false;
    }
    return true;
}


_bsn.AutoSuggest.prototype.killTimeout = function()
{
    clearTimeout(this.toID);
}

_bsn.AutoSuggest.prototype.resetTimeout = function()
{
    clearTimeout(this.toID);
    var pointer = this;
    this.toID = setTimeout(function () { pointer.clearSuggestions() }, 1000);
}







_bsn.AutoSuggest.prototype.clearSuggestions = function ()
{

    this.killTimeout();

    var ele = _bsn.DOM.gE(this.idAs);
    var pointer = this;
    if (ele)
    {
        var fade = new _bsn.Fader(ele,1,0,250,function () { _bsn.DOM.remE(pointer.idAs) });
    }
}










// AJAX PROTOTYPE _____________________________________________


if (typeof(_bsn.Ajax) == "undefined")
    _bsn.Ajax = {}



_bsn.Ajax = function ()
{
    this.req = {};
    this.isIE = false;
}



_bsn.Ajax.prototype.makeRequest = function (url, meth, onComp, onErr)
{

    if (meth != "POST")
        meth = "GET";

    this.onComplete = onComp;
    this.onError = onErr;

    var pointer = this;

    // branch for native XMLHttpRequest object
    if (window.XMLHttpRequest)
    {
        this.req = new XMLHttpRequest();
        this.req.onreadystatechange = function () { pointer.processReqChange() };
        this.req.open("GET", url, true); //
        this.req.setRequestHeader("Accept", "application/xml");
        this.req.send(null);
        // branch for IE/Windows ActiveX version
    }
    else if (window.ActiveXObject)
    {
        this.req = new ActiveXObject("Microsoft.XMLHTTP");
        if (this.req)
        {
            this.req.onreadystatechange = function () { pointer.processReqChange() };
            this.req.open(meth, url, true);
            this.req.setRequestHeader('Accept', "application/xml");
            this.req.send();
        }
    }
}


_bsn.Ajax.prototype.processReqChange = function()
{

    // only if req shows "loaded"
    if (this.req.readyState == 4) {
        // only if "OK"
        if (this.req.status == 200)
        {
            this.onComplete( this.req );
        } else {
            this.onError( this.req.status );
        }
    }
}










// DOM PROTOTYPE _____________________________________________


if (typeof(_bsn.DOM) == "undefined")
    _bsn.DOM = {}



    /* create element */
_bsn.DOM.cE = function ( type, attr, cont, html )
{
    var ne = document.createElement( type );
    if (!ne)
        return false;

    for (var a in attr)
        ne[a] = attr[a];

    if (typeof(cont) == "string" && !html)
        ne.appendChild( document.createTextNode(cont) );
    else if (typeof(cont) == "string" && html)
        ne.innerHTML = cont;
    else if (typeof(cont) == "object"){
        if (cont == null){
            alert(type + ", " + attr + ", " + cont + ", " + html);
        }
        ne.appendChild( cont );
    }

    return ne;
}



/* get element */
_bsn.DOM.gE = function ( e )
{
    if (typeof(e) == "undefined")
        return false;
    else if (typeof(e) == "string")
    {
        var re = document.getElementById( e );
        if (!re)
            return false;
        else if (typeof(re.appendChild) != "undefined" ) {
            return re;
        } else {
            return false;
        }
    }
    else if (typeof(e.appendChild) != "undefined")
        return e;
    else
        return false;
}



/* remove element */
_bsn.DOM.remE = function ( ele )
{
    var e = this.gE(ele);

    if (!e)
        return false;
    else if (e.parentNode.removeChild(e))
        return true;
    else
        return false;
}



/* get position */
_bsn.DOM.getPos = function ( e )
{
    var e = this.gE(e);

    var obj = e;

    var curleft = 0;
    if (obj.offsetParent)
    {
        while (obj.offsetParent)
        {
            curleft += obj.offsetLeft;
            obj = obj.offsetParent;
        }
    }
    else if (obj.x)
        curleft += obj.x;

    var obj = e;

    var curtop = 0;
    if (obj.offsetParent)
    {
        while (obj.offsetParent)
        {
            curtop += obj.offsetTop;
            obj = obj.offsetParent;
        }
    }
    else if (obj.y)
        curtop += obj.y;

    return {x:curleft, y:curtop};
}










// FADER PROTOTYPE _____________________________________________



if (typeof(_bsn.Fader) == "undefined")
    _bsn.Fader = {}





_bsn.Fader = function (ele, from, to, fadetime, callback)
{
    if (!ele)
        return false;

    this.ele = ele;

    this.from = from;
    this.to = to;

    this.callback = callback;

    this.nDur = fadetime;

    this.nInt = 50;
    this.nTime = 0;

    var p = this;
    this.nID = setInterval(function() { p._fade() }, this.nInt);
}




_bsn.Fader.prototype._fade = function()
{
    this.nTime += this.nInt;

    var ieop = Math.round( this._tween(this.nTime, this.from, this.to, this.nDur) * 100 );
    var op = ieop / 100;

    if (this.ele.filters) // internet explorer
    {
        try
        {
            this.ele.filters.item("DXImageTransform.Microsoft.Alpha").opacity = ieop;
        } catch (e) {
            // If it is not set initially, the browser will throw an error.  This will set it if it is not set yet.
            this.ele.style.filter = 'progid:DXImageTransform.Microsoft.Alpha(opacity='+ieop+')';
        }
    }
    else // other browsers
    {
        this.ele.style.opacity = op;
    }


    if (this.nTime == this.nDur)
    {
        clearInterval( this.nID );
        if (this.callback != undefined)
            this.callback();
    }
}



_bsn.Fader.prototype._tween = function(t,b,c,d)
{
    return b + ( (c-b) * (t/d) );
}


// multiword code


// @@ND we just want to do an autocomplete on the last space separated word
_bsn.AutoSuggest.prototype.getLastWord = function (val)
{
    var words = val.split(" ");
    var lastWord = words[words.length-1];
    if (lastWord.charAt(0)=='('){
        lastWord = lastWord.substr(1, lastWord.length-1);
    }

    if (lastWord.charAt(lastWord.length-1)==')'){
        lastWord = lastWord.substr(0, lastWord.length-2);
    }

    return lastWord;
}

// @@ND replace the final word with the new value
_bsn.AutoSuggest.prototype.setLastWord = function (newWord)
{
    var newValue = "";
    var words = this.fld.value.split(" ");
    for (var i=0; i<words.length-1; i++){
        newValue += words[i] + " ";
    }
    if (words[words.length-1].charAt(0)=='('){
        newValue += "(";
    }
    newValue += newWord;
    return newValue;
}
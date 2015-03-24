/****************************************************************
 *
 * parse.js javascript library
 * http://www.nickdrummond.co.uk/code/parse.js/
 *
 * requires: jquery-1.4.3.js (http://jquery.com/)
 * requires: parse-x.x.x.css
 *
 * version:  v0.0.9 beta
 * author:   Nick Drummond (http://www.nickdrummond.co.uk)
 * date:     18 Oct 2010
 * license:  MIT (http://www.opensource.org/licenses/mit-license.html)
 *
 * copyright 2010, Nick Drummond
 *
 ****************************************************************/

/////////////////////////////// debugging flags
DEBUG_RESULTS = false;
DEBUG_KEYS = false;
INLINE_ERROR_HIGHLIGHTER = false; // show the error highlighter after the editor
DEBUG_SHOW_ERROR_DIV = INLINE_ERROR_HIGHLIGHTER || false; // show the error highlighter text
DEBUG_DISABLE_BORDER = false;

// OSX Firefox Ctrl+space brings up context menu
// OSX Chrome Ctrl+space scrolls the main window
// Win XP Alt+space causes window menu to appear on all browsers
USE_CTRL = true;
USE_ALT = true;

// we currently do not support text input fields as there are issues with scrolling and cross browser compat.
SUPPORT_TEXT_INPUT = false;
DEFAULT_ERROR_OFFSET = 10;
AC_TOKEN_CLASS = "actoken";

/////////////////////////////// keycodes
KEY_BACKSPACE = 8;
KEY_RETURN = 13;
KEY_SPACE = 32;
KEY_TAB = 9;
KEY_ESCAPE = 27;
KEY_UP = 38;
KEY_DOWN = 40;
KEY_ALT = 18;
KEY_CTRL = 17;

CSS_SELECTED = "selected";
CSS_INVALID = "invalid";
CSS_ERROR = "error";
CSS_EDITOR = "expression";

// fix IE lack of Array.indexOf
if(!Array.indexOf){
    Array.prototype.indexOf = function(obj, start){
        if (!start){
            start = 0;
        }
        var len = this.length;
        for(var i=start; i<len; i++){
            if(this[i]==obj){
                return i;
            }
        }
        return -1;
    }
}


// store all editors that have been requested, ready for initialisation when the page is finished loading
var editors = new Array();

function register(editor){
    editors.push(editor);
}

$(document).ready(function(){
    //alert($().jquery);

    // initialisation only occurs once the page is fully loaded
    for (var i=0; i<editors.length; i++){
        editors[i].initialise();
    }
});


// The ExpressionEditor object itself.
// There is a single ExpressionEditor per text editor.
function ExpressionEditor(editorId, userOptions){

    var that = this;
    var jEditor;
    var jError;
    var jAutoComplete;
    var jErrorHighlighter;

    this.editorId = editorId;

    // setup default options
    this.options = {
        parser : null,
        autocomplete : null,
        queryParam : "expression",
        highlightResults : true,
        autocompleteOnChange : false,
        errorHandler : function(error){
            if (that.options.parser){
                that.defaultErrorHandler(error);
            }
        }
    };

    // load userOptions into the options
    for (var optName in userOptions){
        this.options[optName] = userOptions[optName];
    }

    register(this);



    // the default error handler just shows the error in a div beneath the editor
    this.defaultErrorHandler = function(error){

        // only show the message if the cursor is on the error
        if (error == null || !cursorIsInError(error)){
            jError.fadeOut("slow");
            return;
        }

        var errorSpan = $(".error", getErrorHighlighter());
        var pos = getSpanPos(errorSpan);

        jError.text(error.message).css({
            "left": pos.x + DEFAULT_ERROR_OFFSET,
            "top": pos.y + DEFAULT_ERROR_OFFSET
        });

        if (!isAutocompleteShowing()){ // show the error panel if autocomplete is not visible
            jError.slideDown("fast");
        }
    };

    this.show = function() {
        jError.fadeOut("fast");
        jAutoComplete.fadeIn("fast");
    };

    this.hide = function() {
        if (jAutoComplete){
            jAutoComplete.fadeOut("fast");
        }
    };

    function requestAutocomplete(expression) {
        makeRequest("autocomplete", expression, that.options.autocomplete, handleAutocomplete, that.autocompleteCache);
    }

    function requestParse(expression) {
        if (jErrorHighlighter){
            jErrorHighlighter.html(""); // clear the error highlighter if it exists
        }
        makeRequest("parse", expression, that.options.parser, handleParse, that.parseCache);
    }

    function makeRequest(name, expression, method, callback, responseCache){

        // check cache for values that have already been used
        var response = responseCache[expression];
        if (response){
            if (DEBUG_RESULTS){
                debug(that.editorId + " " + name + " (cache hit)");
            }
            callback(response);
            showResponse(response.getXML());
        }
        else if (jQuery.isFunction(method)){
            if (DEBUG_RESULTS){
                debug(that.editorId + " " + name + " (function call)");
            }
            response = method(expression);
            responseCache[expression] = response; // cache the responses
            callback(response);
            showResponse(response.getXML());
        }
        else{
            var data = {};
            data[that.options.queryParam] = expression;
            $.ajax({
                url: method,
                data: data,
                context: name, // hacky way of getting the request name in
                datatype: "xml",
                parseCache: false, // don't cache the results (at least not while debugging)
                success: function(xmlData, textStatus, request){
                    if (DEBUG_RESULTS){
                        debug(that.editorId + " " + this + " " + request + " (" + textStatus + ")");
                    }
                    var response = parseResponse(xmlData);
                    responseCache[response.expr] = response; // cache the responses
                    callback(response);
                    showResponse(xmlData);
                },
                error: handleAjaxError});
        }
    }

    function handleAutocomplete(response){
        if (response == null){
            that.hide();
        }
        else{
            that.lastAutocompleteResponse = response;

            var elements = response.expected;

            if (elements.length == 1){
                accept(elements[0], true, true);
            }
            else if (elements.length > 1){

                if (isAutocompleteShowing()){
                    // TODO retain the current width for continuity?
                    jAutoComplete.empty().append(createAutocompleteContent(response));
                }
                else{
                    updateAutocomplete(response);
                    that.show();
                }
            }
        }
    }

    function updateAutocomplete(response){
        var expr = createSpan(AC_TOKEN_CLASS, response.expr, response.pos, response.token.length);
        updateErrorHighlighter(expr);
        jAutoComplete.empty().append(createAutocompleteContent(response));

        var acSpan = $("." + AC_TOKEN_CLASS, getErrorHighlighter());
        var pos = getSpanPos(acSpan);

        var css = {
            "left": pos.x,
            "top": pos.y,
            "position": "absolute",
            "overflow-x" : "hidden",
            "z-index" : 5, // z-index above the error
            "font-family": jEditor.css("font-family"), // use the same font as the editor
            "font-size": jEditor.css("font-size")
        };
        jAutoComplete.css(css);
    }

    function getSpanPos(span){
        var editorPosition = jErrorHighlighter.position(); // the top left of the margin (for some reason)
        var pos = {};
        if (span.length == 1){
            // TODO: need to add offsets for border + margin - padding included in .position() result
            var errorPos = span.position(); // relative to inside top left of editor border (editor is positioned)
            pos.x = editorPosition.left + errorPos.left;
            pos.y = editorPosition.top + errorPos.top + span.height();
        }
        else{
            pos.x = editorPosition.left;
            pos.y = editorPosition.top + jErrorHighlighter.height();
        }
        return pos;
    }

    function createAutocompleteContent(response){
        var list = $("<ul>");

        var types = response.getTypes();

        var first = true;

        for (var i=0; i<types.length; i++){
            var type = types[i];
            var expected = response.getExpected(type);

            for (var j=0; j<expected.length; j++){
                var element = $("<li>");
                element.addClass(type);
                if (first){
                    element.addClass(CSS_SELECTED);
                    first = false;
                }

                // add a mouse click handler for the element
                element.click(function(){
                    acceptAutocomplete(false, $(this));
                });

                var elementText = expected[j];

                if (that.options.highlightResults && response.token){
                    var matchlen = response.token.length;
                    elementText = "<span class=\"match\">" + elementText.substr(0, matchlen) +
                                  "</span>" + elementText.substring(matchlen, elementText.length);
                }

                element.html(elementText);

                list.append(element);
            }
        }
        return list;
    }

    function handleParse(response) {
        if (typeof response.pos === "undefined"){ // hack to see if this is an error
            response = null;
        }

        if (response){
            if (!DEBUG_DISABLE_BORDER){
                jEditor.addClass(CSS_INVALID);
            }
            var expr = createSpan(CSS_ERROR, response.expr, response.pos, response.token.length);

            updateErrorHighlighter(expr);
        }
        else{
            if (!DEBUG_DISABLE_BORDER){
                jEditor.removeClass(CSS_INVALID);
            }
        }

        if (that.options.errorHandler){
            that.options.errorHandler(response);
        }
    }

    function updateErrorHighlighter(expr){
        expr = encode(expr);
        expr = expr.replace(/ (?= )/g, "&nbsp;");   // replace multiple spaces (but still allow the last for wrap)
        expr = expr.replace(/\n /g, "<br/>&nbsp;"); // newline followed by space should retain space      
        expr = expr.replace(/\n/g, "<br/>");        // wrap in the appropriate places
        expr = expr.replace(/%%AMP%%/g, "&");
        expr = expr.replace(/%%LT%%/g, "<");
        expr = expr.replace(/%%GT%%/g, ">");
        expr = expr.replace(/%%SP%%/g, " ");
        expr = expr.replace(/%%QU%%/g, "\"");
        expr = expr + "<br/>";                      // add a br at the end as the div will ignore any others otherwise

        getErrorHighlighter().html(expr);
        syncScroll(null);                           // force the scrollers to sync in case a newline has been added
    }

    function getErrorHighlighter(){
        if (!jErrorHighlighter){
            jErrorHighlighter = createErrorHighlighter(); // lazy create a div under the editor for error highlighting
        }
        return jErrorHighlighter
    }

    function handleAjaxError(request, textStatus, errorThrown){
        if (!errorThrown){
            errorThrown = "Service failed to satisfy this request";
        }
        debug(that.editorId + " (" + request.statusText + "): " + errorThrown);
    }

    function cursorIsInError(error){
        var sel = getSelectionStart(that.editor);
        return sel >= error.pos && sel <= error.pos + error.token.length;
    }

    function getSelection(){
        return $("li." + CSS_SELECTED, jAutoComplete);
    }

    function next(){
        var sel = getSelection();
        if (sel.next().length > 0){
            handleSelection(sel.next());
        }
    }

    function prev(){
        var sel = getSelection();
        if (sel.prev().length > 0){
            handleSelection(sel.prev());
        }
    }

    function handleSelection(newSel){
        getSelection().removeClass(CSS_SELECTED);
        newSel.addClass(CSS_SELECTED);
        scroll(newSel, null);
    }

    function acceptAutocomplete(replaceCurrent, selected) {
        if (!selected){
            selected = getSelection();
        }
        accept(selected.text(), false, replaceCurrent);
        skipToNextWord(that.editor, false);
        jEditor.focus();
    }

    function accept(word, highlight, replaceWholeWord) {
        that.hide();
        var editor = that.editor;
        // must use the error to determine the start of the token we'll replace in cases where there is no space delimeter
        var pos = that.lastAutocompleteResponse.pos;
        var tLen = getSelectionStart(editor) - pos;

        if (replaceWholeWord){
            replaceWord(editor, pos, word);
        }
        else{
            replace(editor, pos, pos+tLen, word);
        }

        if (highlight){
            setSel(editor, pos + tLen, word.length-tLen);
            that.currentTokenHighlighted = true;
        }
        else{
            setSel(editor, pos + word.length, 0); // move the caret to the end of the word
        }

        requestParse(jEditor.val());
    }

    // create a div for the error highlighting to be created inside
    function createErrorHighlighter() {

        // save the background colour as we're about to wipe it out
        var backgroundColour = jEditor.css("background-color");

        // make the editor transparent
        // have to do this before the css copy as it affects the borders etc on Firefox
        jEditor.css({
            "background-color": "transparent"
        });

        var css = copyCSS(jEditor);

        css["background-color"] = backgroundColour;
        css["display"] = "inline-block";            // needs to be inline-block as textarea is inline
        css["z-index"] = getEditorZindex()-1;       // below the editor
        if (DEBUG_SHOW_ERROR_DIV){
            css["color"] = "blue";
        }
        else{
            // set the text the same as the background to make it invisible
            css["color"] = backgroundColour;
        }

        // webkit seems to have auto instead of inherit? (which it renders as left aligned)
        if (css["text-align"] == "auto"){
            css["text-align"] = "left";
        }

        // textareas with visible overflow act like auto (at least in FF)
        if (css["overflow-x"] == "visible"){
            css["overflow-x"] = "auto";
        }
        if (css["overflow-y"] == "visible"){
            css["overflow-y"] = "auto";
        }

        // wrap is not in css for textarea - it is set in the wrap attribute
        if (jEditor.attr("wrap") == "off"){
            css["white-space"] = "nowrap";
        }
        else{
            // TODO: how do we handle automatic breaking of long words performed by textarea?
        }

        appendOverlay(jEditor, css);

        var highlighter = $("<div id=\"" + that.errorHighlightId + "\" class=\"errorhighlight\"></div>").css(css);

        highlighter.insertAfter(jEditor);

        return highlighter;
    }

    function getEditorZindex(){
        var zIndex = jEditor.css("z-index");
        if (zIndex == "auto"){
            zIndex = "0";
        }
        return parseInt(zIndex);
    }

    function createErrorPane(){
        return $("<div>").addClass("errormessage").css("z-index", getEditorZindex() + 1).hide().insertAfter(jEditor);
    }

    function prepareEditor() {

        // make sure the editor has the class expression for css
        jEditor.addClass(CSS_EDITOR);

        // make sure the spellchecker is turned off for firefox
        jEditor.attr("spellcheck", false);

        var css = {
            // turn off the blue glow by reapplying the background (for some reason)
            "background-color": jEditor.css("background-color"), // for Firefox
            "outline": "none",                                   // for Safari

            // turn off resize in Safari/Chrome
            //            "resize": "none",

            // position the editor above the error highlighter
            "z-index": "1",

            // force IE to report the correct font in css
            // if there is no font specified it uses monospace but reports the inherited font
            "font-family": jEditor.css("font-family")
        };

        // TODO: is there some way to do this that does not require changes to the editor css?
        // Unfortunately, can't use errHigh.css(z-index:-1) as this goes behind the parent
        // must make the editor higher than the error highlighter
        if (jEditor.css("position") == "static"){
            css["position"] = "relative"; // z-index will only work on positioned elements
        }

        jEditor.css(css);

        // hide the autocompleter when focus is lost
        jEditor.blur(function(event){
            that.hide();
            jError.fadeOut("slow");
        });

        // register a handler to synchronise the scroll positions of the editor and error highlighter
        jEditor.scroll(syncScroll);

        // if the window is resized or other elements change, the editor is moved
        // this handles the window resize
        $(window).resize(function(){
            if (jErrorHighlighter){
                overlay(jErrorHighlighter, jEditor);
            }
            that.hide();
        });

        // and this handles programmatic changes to the box model
        jEditor.watch('left,top,width,height', function(){
            if (jErrorHighlighter){
                overlay(jErrorHighlighter, jEditor);
            }
        });

        setupKeys();
    }

    function syncScroll(event){
        if (jErrorHighlighter){ // only sync if the highlighter exists
            jErrorHighlighter.scrollTop(jEditor.scrollTop());
            jErrorHighlighter.scrollLeft(jEditor.scrollLeft());
        }
    }

    function isAutocompleteShowing(){
        return jAutoComplete && jAutoComplete.is(":visible");
    }

    function isAutocompleteTrigger(event){
        if (event.which == KEY_SPACE){
            return (USE_CTRL && event.ctrlKey) || (USE_ALT && event.altKey);
        }
        return event.which == 160;
    }

    // keypress is supposed to be used only for keys that input characters
    // see http://unixpapa.com/js/key.html

    // in Firefox up/down should use keypresses for key repeat to work but these are not fired for up/down in Safari/Chrome
    // tab produces keypress event.which=0 in Firefox

    // Autocomplete key events in different browsers
    // Firefox keydown 18+alt,  keydown 32+alt, keypress 160,     keyup 32+alt,  keyup 18
    // Opera   keydown 18,                      keypress 160,                    keyup 18
    // Safari  keydown 18+alt,  keydown 32+alt, keypress 160+alt, keyup 32+alt,  keyup 18
    // Chrome  keydown 18+alt,  keydown 32+alt, keypress 160+alt, keyup 32+alt,  keyup 18
    // IE6/7   keydown 17+ctrl, keydown 32+ctrl,                  keyup 32+ctrl, keyup 17

    function setupKeys(){
        jEditor.keypress(function(event){
            if (DEBUG_KEYS){
                debug("keypress:   " + event.which + " (alt=" + event.altKey + ", ctrl=" + event.ctrlKey + ", currentTokenHighlighted=" + that.currentTokenHighlighted + ")");
            }

            if (isAutocompleteTrigger(event)){
                if (!jEditor.doingAutocomplete){ // handle Opera event
                    var expr = this.value.substr(0, getSelectionStart(this));
                    requestAutocomplete(expr);
                }
                event.preventDefault();
            }
            else if (isAutocompleteShowing()){
                if (event.which == 0){ // Firefox gets a 0 for repeats of the up/down keys
                    if (that.repeat){ // ignore the first keypress
                        // so we need to check if the last key down was one of these and act
                        if (that.lastKey == KEY_DOWN){
                            next();
                            event.preventDefault();
                        }
                        else if (that.lastKey == KEY_UP){
                            prev();
                            event.preventDefault();
                        }
                    }
                    that.repeat = true;
                }
                else if (event.which == KEY_TAB){ // required to prevent Opera from changing focus
                    event.preventDefault();
                }
            }
        });

        // create key bindings
        jEditor.keyup(function(event){
            if (DEBUG_KEYS){
                debug("keyup:   " + event.which + " (alt=" + event.altKey + ", ctrl=" + event.ctrlKey + ", currentTokenHighlighted=" + that.currentTokenHighlighted + ")");
            }

            var newValue = this.value;
            var textChanged = (newValue != that.lastValue);
            that.lastValue = newValue;

            if (isAutocompleteTrigger(event)){
                jEditor.doingAutocomplete = false;
                event.preventDefault();
            }
            else if (isAutocompleteShowing()){
                if (event.which == KEY_BACKSPACE && that.options.autocompleteOnChange){
                    that.hide();
                }
                else if (event.which == KEY_RETURN){
                    acceptAutocomplete(false, null);
                    event.preventDefault();
                }
                else if (event.which == KEY_ESCAPE){
                    that.hide();
                    event.preventDefault();
                }
                else if (event.which == KEY_TAB){
                    acceptAutocomplete(true, null);
                    textChanged = (newValue != that.lastValue);
                    that.lastValue = newValue;
                    event.preventDefault();
                }
                else if (event.which == KEY_DOWN || event.which == KEY_UP){
                    // do nothing - handled by keydown/press
                }
                else {
                    requestAutocomplete(newValue.substr(0, getSelectionStart(this)));
                }
            }
            else if (event.which == KEY_BACKSPACE){ // backspace prevents the autocompleter
                // TODO: actually, we just want to prevent the auto-accept

            }
            else if (that.currentTokenHighlighted){ // then the current selection is due to an autocomplete
                if (event.which == KEY_SPACE){
                    skipToNextWord(this, true);
                    event.preventDefault();
                }
                else if (event.which == KEY_RETURN){
                    skipToNextWord(this, false);
                    event.preventDefault();
                }
                else if (that.options.autocompleteOnChange && textChanged){
                    requestAutocomplete(newValue.substr(0, getSelectionStart(this)));
                }

                if (event.which != KEY_ALT && event.which != KEY_CTRL){ // ignore releasing the alt/ctrl key from ac request - bit of a hack
                    that.currentTokenHighlighted = false;
                }
            }
            else if (that.options.autocompleteOnChange && textChanged){
                requestAutocomplete(newValue.substr(0, getSelectionStart(this)));
            }

            if (textChanged){
                requestParse(jEditor.val());
            }
        });


        // block all keydown defaults for used bindings
        jEditor.keydown(function(event){

            that.lastKey = event.which;
            that.repeat = false;

            if (DEBUG_KEYS){
                debug("keydown: " + event.which + " (alt=" + event.altKey + ", ctrl=" + event.ctrlKey + ", currentTokenHighlighted=" + that.currentTokenHighlighted + ")");
            }

            if (isAutocompleteTrigger(event)){
                jEditor.doingAutocomplete = true;
                var expr = this.value.substr(0, getSelectionStart(this));
                requestAutocomplete(expr);
                event.preventDefault();
            }
            else if (isAutocompleteShowing()){
                // key repeats in Safari/Chrome cause multiple keydown events
                if (event.which == KEY_DOWN){
                    next();
                    event.preventDefault();
                }
                else if (event.which == KEY_UP){
                    prev();
                    event.preventDefault();
                }
                else if (event.which == KEY_RETURN ||
                         event.which == KEY_ESCAPE ||
                         event.which == KEY_TAB){
                    event.preventDefault();
                }
            }
            else if (that.currentTokenHighlighted){
                if (event.which == KEY_SPACE ||
                    event.which == KEY_RETURN){
                    event.preventDefault();
                }
            }
        });
    }

    this.toString = function(){
        return this.autocompleteId;
    };

    this.initialise = function(){

        // find a textarea or text input with the id
        var query = "textarea#" + this.editorId;
        if (SUPPORT_TEXT_INPUT){
            query += ", input#" + this.editorId + "[type='text']";
        }
        jEditor = $(query);

        if (!jEditor || jEditor.length < 1){
            // in FF this shows an error in the error console - is this enough?
            throw("Cannot find a text component with id: " + this.editorId);
        }

        this.editor = jEditor.get(0);

        this.autocompleteId = "autocomplete_for_" + editorId;
        this.errorHighlightId = "error_highlight_for_" + editorId;

        this.lastValue = this.editor.value; // only used for autocompleteOnChange
        this.lastAutocompleteResponse = null; // needed for the accept to work
        this.currentTokenHighlighted = false;

        this.parseCache = new Array();        // map of expression -> parse results
        this.autocompleteCache = new Array(); // map of expression -> ac results

        prepareEditor();

        jAutoComplete = $("<div class=\"autocomplete\"/>").insertAfter(jEditor);

        if (this.options.parser){
            // create a div for the error message for the default error handler
            jError = createErrorPane();

            // if there is already content in the editor, we need to perform a parse
            if (jEditor.val().length > 0){
                requestParse(jEditor.val());
            }
        }
    }
}

////////////////////////    Responses   ///////////////////////////////////////


function parseResponse(xmlData){
    if (typeof xmlData == 'string'){ // if this is plain text XML
        xmlData = parseXML(xmlData);     // if so, convert it into a dom
    }

    var root = $("error", xmlData);

    if (root.length > 0){
        var err = new ParseError($("expression", root).text(),
                                 $("message", root).text(),
                                 parseInt(root.attr("pos")),
                                 root.attr("found"));
        err.xml = xmlData;
        return err
    }

    root = $("success", xmlData);

    if (root.length > 0){
        var success = new ParseSuccess($("expression", root).text(),
                                       $("message", root).text());
        success.xml = xmlData;
        return success;
    }

    root = $("results", xmlData);
    if (root.length > 0){

        var acResult = new AutocompleteResult($("expression", root).text(),
                                              parseInt(root.attr("pos")),
                                              root.attr("found"));

        $("expected", root).each(function(){
            var type = $(this).attr("type");
            var expected = new Array();
            $("token", $(this)).each(function(){
                expected.push($(this).text());
            });
            acResult.addExpected(type, expected);
        });

        acResult.xml = xmlData;
        return acResult;
    }

    throw new Error("Unknown response type: " + xmlData.child);
}

/**
 * The ParseError object abstracts from the xml representation returned from the server.
 * In future we could easily add JSON as an alternative
 * @param expr
 * @param message
 * @param pos
 * @param token
 */
function ParseError(expr, message, pos, token){

    this.expr = expr;
    this.message = message;
    this.pos = pos;
    this.token = token;

    this.getXML = function(){
        if (!this.xml){
            this.xml = parseXML("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
                                "\n<error pos=\"" + this.pos + "\" found=\"" + encode(this.token) + "\">" +
                                "\n    <expression>" + encode(this.expr) + "</expression>" +
                                "\n    <message>" + encode(this.message) + "</message>" +
                                "\n</error>");
        }
        return this.xml;
    };
}

function ParseSuccess(expr, message){

    this.expr = expr;
    this.message = message;

    this.getXML = function(){
        if (!this.xml){
            this.xml = parseXML("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
                                "\n<success>" +
                                "\n    <expression>" + encode(this.expr) + "</expression>" +
                                "\n    <message>" + encode(this.message) + "</message>" +
                                "\n</success>");
        }
        return this.xml;
    };
}

function AutocompleteResult(expr, pos, token){

    this.expr = expr;                     // the expression that AC was performed against
    this.pos = pos;                       // the position of the start of the token
    this.token = token;                   // the token that AC was performed against (as this may not be space delimited)
    this.expected = new Array();          // all expected values
    this.types = new Array();             // all types
    this.expectedByType = new Array();    // expected values by type

    this.getXML = function(){
        if (!this.xml){
            this.xml = parseXML("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
                                "\n<results pos=\"" + this.pos + "\" found=\"" + encode(this.token) + "\">" +
                                "\n    <expression>" + encode(this.expr) + "</expression>" +
                                "\n    <expected>TODO Serialise this " + encode(this.expected) + "</expected>" +
                                "\n</results>");
        }
        return this.xml;
    };

    this.addExpected = function(type, values){
        if (this.expectedByType[type]){
            pushAll(values, this.expectedByType[type]);
        }
        else{
            this.expectedByType[type] = values;
            this.types.push(type);
        }
        pushAll(values, this.expected);
    };

    this.getExpected = function(type){
        if (type){
            return this.expectedByType[type];
        }
        else{
            return this.expected;
        }
    };

    this.getTypes = function(){
        return this.types;
    }
}


////////////////////////    Utilities   ///////////////////////////////////////


function parseXML(xml) {
    if(window.ActiveXObject && window.GetObject) {
        var dom = new ActiveXObject('Microsoft.XMLDOM');
        dom.loadXML(xml);
        return dom;
    }
    else if(window.DOMParser){
        return new DOMParser().parseFromString(xml, 'text/xml');
    }
    throw new Error('No XML parser available');
}

// revisit all of this by looking at Range (http://www.quirksmode.org/dom/range_intro.html)

function replaceInString(str, start, end, word){
    //    debug("replace (" + start + ", " + end + ") with \"" + word + "\" in string \"" + str + "\"");
    return str.substr(0, start) + word + str.substring(end, str.length);
}

function insertIntoString(str, pos, word){
    return replaceInString(str, pos, pos, word);
}

function replaceWordInString(str, pos, word){ // replace the word containing the position pos
    var endOfWord = str.indexOf(" ", pos);
    if (endOfWord < 0){
        endOfWord = str.length;
    }
    return replaceInString(str, pos, endOfWord, word);
}

// Some utility methods for handling text editors.
// These should probably be added to the appropriate editor on init.
function insert(editor, pos, word){
    editor.value = insertIntoString(editor.value, pos, word);
}

function replace(editor, start, end, word){
    editor.value = replaceInString(editor.value, start, end, word);
}

function replaceWord(editor, pos, word){
    editor.value = replaceWordInString(editor.value, pos, word);
}


function createSpan(cssClass, string, start, length){
    var end = start + length;

    var openSpan = "%%LT%%span%%SP%%class=%%QU%%" + cssClass + "%%QU%%%%GT%%";
    var closeSpan = "%%LT%%/span%%GT%%";

    if (start == end){                                           // if end of line
        string = string + openSpan + "%%AMP%%nbsp;" + closeSpan; // add a trailing space for the error highlight
    }
    else{                                                        // insert error span tags
        string = insertIntoString(string, end, closeSpan);       // in reverse order so not to mess up the indices
        string = insertIntoString(string, start, openSpan);
    }
    return string;
}

function setSel(editor, pos, length) {
    var end = pos+length;
    if (editor.setSelectionRange){
        editor.setSelectionRange(pos, end);
    }
    else if (editor.createTextRange) {
        var range = editor.createTextRange();
        range.collapse(true);
        range.moveEnd('character', end);
        range.moveStart('character', pos);
        range.select();
    }
    else{
        throw("failed to set sel range");
    }
}

function getSel(){
    // see http://the-stickman.com/web-development/javascript/finding-selection-cursor-position-in-a-textarea-in-internet-explorer/
    if (window.selection){ // IE
        return window.selection.createRange();
    }
    else if(document.selection){ // Opera
        return document.selection.createRange();
    }
    throw("Cannot get selection start for editor");
}

function getSelectionStart(editor) {
    if (editor.selectionStart){
        return editor.selectionStart;
    }

    var range = getSel();
    var stored_range = range.duplicate();
    stored_range.moveToElementText(editor);
    stored_range.setEndPoint( 'EndToEnd', range );
    return stored_range.text.length - range.text.length;
}

function getSelectionEnd(editor) {
    if (editor.selectionEnd){
        return editor.selectionEnd;
    }

    var range = getSel();
    var stored_range = range.duplicate();
    stored_range.moveToElementText(editor);
    stored_range.setEndPoint( 'EndToEnd', range );
    var start = stored_range.text.length - range.text.length;
    return start + range.text.length;
}

function skipToNextWord(editor, addSpace) {
    var sel = getSelectionEnd(editor);
    var nextSpace = editor.value.indexOf(" ", sel); // the space after the current word

    if (nextSpace == -1){
        if (addSpace){
            editor.value = editor.value + " "; // add the space
        }
        nextSpace = editor.value.length;
    }
    setSel(editor, nextSpace+1, 0);
}

function getStartOfSelectedWord(editor) {
    var sel = getSelectionStart(editor);
    if (sel == -1){
        return 0;
    }
    var str = editor.value;
    return str.lastIndexOf(" ", str.substr(0, sel)) + 1; // the space before the current word
}

function scroll(element, scrollParent){

    if (!scrollParent){
        scrollParent = element.offsetParent();
    }

    var elementTop = element.position().top;
    var offset = elementTop - element.parent().position().top;

    if (elementTop < 0){
        scrollParent.scrollTop(offset);
    }
    else{
        var scrollerHeight = scrollParent.height();
        var elementHeight = element.height();
        if (elementTop + elementHeight > scrollerHeight){
            scrollParent.scrollTop(offset + elementHeight - scrollerHeight);
        }
    }
}

function scrollBottom(element){
    element.attr("scrollTop", element.attr("scrollHeight"));
}

// move element to the same position
function overlay(element, target){
    element.css(getOverlay(target));
}

function getOverlay(target){
    var css = {};
    appendOverlay(target, css);
    return css;
}

function appendOverlay(target, css){
    css["width"] =  target.width();
    css["height"] =  target.height();

    if (INLINE_ERROR_HIGHLIGHTER){  // for debugging
        css["position"] = "static";
    }
    else {
        css["position"] = "absolute";
        css["top"] =  target.position().top;
        css["left"] =  target.position().left;
    }
}

function copyCSS(element) {

    // styles from Firebug's computed styles inspector
    var styles = new Array(
        // text
            "font-family",
            "font-size",
            "font-weight",
            "font-style",
            "color",
            "text-transform",
            "text-decoration",
            "letter-spacing",
            "word-spacing",
            "line-height",
            "text-align",
            "vertical-align",
            "direction",

        // background
            "background-color",
            "background-image",
            "background-repeat",
            "background-position",
            "background-attachment",
            "opacity",

        // box model
            "width",
            "height",
            "top",
            "right",
            "bottom",
            "left",
            "margin-top",
            "margin-right",
            "margin-bottom",
            "margin-left",
            "padding-top",
            "padding-right",
            "padding-bottom",
            "padding-left",
            "border-top-width",
            "border-right-width",
            "border-bottom-width",
            "border-left-width",
            "border-top-color",
            "border-right-color",
            "border-bottom-color",
            "border-left-color",
            "border-top-style",
            "border-right-style",
            "border-bottom-style",
            "border-left-style",

        // layout
            "position",
            "display",
            "visibility",
            "z-index",
            "overflow-x",
            "overflow-y",
            "white-space",
            "clip",
            "float",
            "clear",
            "-moz-box-sizing",
            "-moz-appearance", // for Firefox "glow"
            "-moz-border-radius", // for Firefox rounded corners
            "resize",

        // other
            "cursor",
            "list-style-image",
            "list-style-position",
            "list-style-type",
            "marker-offset"
            );

    var css = {};

    for (var i=0; i<styles.length; i++){
        var style = styles[i];
        css[style] = element.css(style);
    }

    return css;
}

function encode(string){
    if (string && string.replace){
        string = string.replace(/&/g, "&amp;");
        string = string.replace(/</g, "&lt;");
        string = string.replace(/>/g, "&gt;");
        string = string.replace(/"/g, "&quot;");
    }

    return string;
}

function pushAll(source, target){
    for (var i=0; i<source.length; i++){
        target.push(source[i]);
    }
}

///////////////// debug info ////////////////////////////

function showResponse(xmlData) {
    var xmlConsole = $("#xml");
    if (xmlConsole && xmlConsole.length > 0){
        var message;
        try{
            message = new XMLSerializer().serializeToString(xmlData);
        }
        catch(e){
            // IE
            message = xmlData.xml;
        }
        message = encode(message);
        xmlConsole.html(message);
    }
}

function debug(message) {
    var console = $("#debug");
    if (console && console.length > 0){

        message = encode(message);

        console.append("<br/>" + message);

        scrollBottom(console);
    }
}

//////////////// test parser implementation //////////////////

var dictionary = new Array("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                           "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty");

function TestParser(expression){

    var pos = 0;
    var unrecognized = null;

    var tokens = expression.replace(/\n/g, " ").split(" "); // replace all newlines
    for (var i=0; i<tokens.length; i++){
        var token = tokens[i];
        if (dictionary.indexOf(token) == -1){
            unrecognized = encode(token);
            break;
        }
        pos+=token.length + 1;
    }

    if (!unrecognized){
        return new ParseSuccess(expression, "OK");
    }
    else{
        var message = "Expected the name of a Number";
        return new ParseError(expression, message, pos, unrecognized);
    }
}

function TestAutocomplete(expression){

    var lastSpace = expression.replace(/\n/g, " ").lastIndexOf(" ")+1;
    var lastWord = expression.substring(lastSpace, expression.length);

    var matchingTerms = new Array();

    for (var i=0; i<dictionary.length; i++){
        var term = dictionary[i];
        if (term.match("^" + lastWord)){
            matchingTerms.push(term);
        }
    }

    var result = new AutocompleteResult(expression, lastSpace, lastWord);
    result.addExpected("Number", matchingTerms);
    return result;
}


// DOM changes event handling
// thanks to Darcy Clarke (http://darcyclarke.me/development/detect-attribute-changes-with-jquery/)
// inspired by Rick Strahl (http://www.west-wind.com/Weblog/posts/478985.aspx)

$.fn.watch = function(props, callback, timeout){
    if(!timeout)
        timeout = 10;
    return this.each(function(){
        var el      = $(this),
                func    = function(){ __check.call(this, el) },
                data    = { props:  props.split(","),
                    func:   callback,
                    vals:   [] };
        $.each(data.props, function(i) { data.vals[i] = el.css(data.props[i]); });
        el.data(data);
        if (typeof (this.onpropertychange) == "object"){
            el.bind("propertychange", callback);
        } else if ($.browser.mozilla){
            el.bind("DOMAttrModified", callback);
        } else {
            setInterval(func, timeout);
        }
    });
    function __check(el) {
        var data    = el.data(),
                changed = false,
                temp    = "";
        for(var i=0;i < data.props.length; i++) {
            temp = el.css(data.props[i]);
            if(data.vals[i] != temp){
                data.vals[i] = temp;
                changed = true;
                break;
            }
        }
        if(changed && data.func) {
            data.func.call(el, data);
        }
    }
}

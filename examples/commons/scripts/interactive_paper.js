// insert tool menu

function toolMenu() {
  var menu_div = document.createElement("div");
  menu_div.setAttribute("id", "tool_div");
  var menu = document.createElement("div");
  menu.setAttribute("id", "tool_menu");
  menu_div.appendChild(menu);

  var layoutB = document.createElement("button");
  layoutB.innerHTML = "Switch to ACM layout";
  layoutB.setAttribute("id", "layout_button");
  // layoutB.disabled = true;
  layoutB.addEventListener('click', function(){
    switchLayout();
  }, false);
  menu.appendChild(layoutB);

  var resetB = document.createElement("button");
  resetB.innerHTML = "<u>D</u>efault analysis";
  resetB.setAttribute("id", "reset_button");
  // resetB.disabled = true;
  resetB.addEventListener('click', function(){
    tangle.updateModel(true);
  }, false);
  menu.appendChild(resetB);

  var animateB = document.createElement("button");
  animateB.innerHTML = "<u>A</u>nimate multiverse";
  animateB.setAttribute("id", "animate_button");
  animateB.disabled = true;
  animateB.addEventListener('mousedown', function(){
    animateMultiverse();
  }, false);
  animateB.addEventListener('mouseup', function(){
    stopAnimation();
  }, false);
  menu.appendChild(animateB);

  var infoField = document.createElement("div");
  infoField.innerHTML = "Loading images... Animation unavailable while loading...";
  infoField.setAttribute("id", "loading_info");
  infoField.addClass("loading_info");
  menu.appendChild(infoField);


  $('#tangleRoot').prepend(menu_div);


}
  // // layout doesn't work in all browsers the same so we add tags to customize the css to the user's browser
// Opera 8.0+
var isOpera = (!!window.opr && !!opr.addons) || !!window.opera || navigator.userAgent.indexOf(' OPR/') >= 0;

// Firefox 1.0+
var isFirefox = typeof InstallTrigger !== 'undefined';

// Safari 3.0+ "[object HTMLElementConstructor]" 
var isSafari = /constructor/i.test(window.HTMLElement) || (function (p) { return p.toString() === "[object SafariRemoteNotification]"; })(!window['safari'] || (typeof safari !== 'undefined' && safari.pushNotification));

// Internet Explorer 6-11
var isIE = /*@cc_on!@*/false || !!document.documentMode;

// Edge 20+
var isEdge = !isIE && !!window.StyleMedia;

// Chrome 1+
var isChrome = (!!window.chrome && !!window.chrome.webstore) || /Google Inc/.test(navigator.vendor);


// Blink engine detection
var isBlink = (isChrome || isOpera) && !!window.CSS;


var anim = null;

function animateMultiverse(){
    if (typeof anim == 'undefined' || anim == null) {
      tangle.randomize();
      anim = setInterval(tangle.randomize, 120);  
    }
}

function stopAnimation(){
    clearInterval(anim);
    anim = null;  
}

// enable keyboard shortcut for animation
window.onkeydown = function (e) {
    var code = e.keyCode ? e.keyCode : e.which;
    if (code === 65) animateMultiverse();
    if (code === 68) tangle.updateModel(true);
};
window.onkeyup = function (e) {
    var code = e.keyCode ? e.keyCode : e.which;
    if (code === 65) stopAnimation();
};


// this function takes the parent tag and adds "letter" divs to create a page layout
function snip(){
    var max_pages = 20;
    var page_count = 1;    // get parent tag containing our artice
    var externalTag = $('dt-article');
    // create array with all child nodes for our article
    var children = externalTag.children().toArray();
    // create a div that is laid out as a page and add all children to it
    var letterDiv = document.createElement('div');
    letterDiv.className = "letter";
    while (externalTag.children().length > 0) {
        letterDiv.appendChild($(externalTag)[0].firstChild);
    }
    // append new div to parent node
    externalTag.append(letterDiv);

    while(children.length > 0 && page_count <= max_pages){
        page_count++;
        // first while loop which creates new pages as long as we have children left to place
        var newpage = document.createElement('div');
        newpage.className = "letter";
        // insert the new page node
        externalTag.append(newpage);
        // variables to store the current length of our new page in
        var long = letterDiv.scrollHeight - letterDiv.clientHeight;
        var wide = $(letterDiv).prop('scrollWidth') - Math.ceil($(letterDiv).innerWidth());
        while ((long > 0 || wide > 0) && children.length > 0) {
            var child = children.pop();
            $(child).detach();
            newpage.prepend(child);
            if ($(child).innerHeight() != 0){
                long = $(letterDiv).prop('scrollHeight') - Math.ceil($(letterDiv).innerHeight());

                wide = $(letterDiv).prop('scrollWidth') - Math.ceil($(letterDiv).innerWidth());
            }
        }
        // check if page ended with a new header, if so, move it to the next page
        if ($(letterDiv).children().last().is("h2") || $(letterDiv).children().last().is("h3")){
            var heading = $(letterDiv).children().last();
            $(heading).detach();
            $(newpage).prepend(heading);
        }


        letterDiv = newpage;
        children = $(newpage).children().toArray();

    }
    if ($(letterDiv).length){
        $(letterDiv).detach();
    }

}

function unsnip(){
	$('.letter, .p1col1, .p1col2').each(function() {
		var str = $(this).html();
		$(this).before(str);
		$(this).empty();
		$(this).detach();
	});

}


// define method to loa da cached script
jQuery.cachedScript = function( url, options ) {
 
  // Allow user to set any option except for dataType, cache, and url
  options = $.extend( options || {}, {
    dataType: "script",
    cache: true,
    url: url
  });
 
  // Use $.ajax() since it is more flexible than $.getScript
  // Return the jqXHR object so we can chain callbacks
  return jQuery.ajax( options );
};
 


// currently the first layout is set to distill since it generates the references
var currentLayout = "distill";
var distill_refs, acm_refs;
var ref_header;
var distill_authors, acm_authors, title;



function switchLayout() {
  var url = window.location.href;
if (isSafari && (url.contains("dance") || url.contains("likert") || url.contains("prior"))){
  alert('Unfortunately this example does not work with the ACM layout in Safari. We only fully tested all examples in Chrome. If you do not have Chrome, you can look at the ACM layout version of the examples "Frequentist" or "Dataverse".');
} else {
  console.log("switching layouts...");
  $('#layout_button').prop('disabled', true);
  if (tangle) {
    tangle.cleanup();
  }
  tangle = null;

  // switching to distill layout
  if (currentLayout == "ACM"){
    // disable ACM css
    $('link[href$="pubcss-acm-sigchi.css"]').prop('disabled', true);
    $('link[href$="style.css"]').prop('disabled', true);

    // remove page divs
    unsnip();
    // show distill style references and authors
    $(acm_refs).hide();
    $('.distill-bib').show();
    $('.acm-bib').hide();
    $('dt-byline').show();
    $('header').hide();
    $('#acm, #terms').hide();
    $('dt-article').prepend($('.title'));

    // setup tangle for the interactive text
    setUpTangle();
    // get the distill js
    // $.getScript("https://distill.pub/template.v1.js");
    $.cachedScript( "https://distill.pub/template.v1.js" ).done(function( script, textStatus ) {
      $('#layout_button').prop('disabled', false);
      console.log( "reloaded distill");
    });
      moveFullColumnFiguresBack();

    $('style').prop('disabled', false);

    // store that we switched layout
    currentLayout = "distill";
    $('#layout_button').html('Switch to ACM layout');

  } else {
    // switching to ACM layout
    if (isFirefox) {
      alert('Unfortunately Firefox does not display content correctly that spans two columns. You will therefore experience overlapping content and double column figures squeezed into one column when switching to the ACM layout. For a fully compatible viewing experience, please consider using Chrome.');
    } 
    setUpTangle();
    
    // remove distill js file
    // removejscssfile("template.v1.js", "js");
    $('style').prop('disabled', true);

    $('link[href$="pubcss-acm-sigchi.css"]').prop('disabled', false);

    $('link[href$="style.css"]').prop('disabled', false);
    // show ACM style references and authors
    $('.distill-bib').hide();
    $('.acm-bib').show();
    // $(acm_authors).show();
    $('header').show();
    $('dt-byline').hide();
    $('#acm, #terms').show();
    $('header').prepend($('.title'));

    // insert page divs

    snip();

    moveFullColumnFiguresToTopOfPage();

    currentLayout = "ACM";
    $('#layout_button').html('Switch to Distill layout');
    $('#layout_button').prop('disabled', false);
  }
}

    // Dispatch an event
$(document).trigger('layoutChanged');
console.log("switched layout to " + currentLayout);

}


// setup to enable switching between layouts;
// - makes sure that the references created by distill are available
//   for the ACM layout as well;
// - assigns the distill and ACM versions of author listing to variables
//   to show and hide them when switching
$(document).ready(function(){
  toolMenu();
  setUpTangle(); 
  console.log('Tangle set up.');
  var images = new Array();
  var promises = new Array();
  function preload() {
    for (i = 0; i < preload.arguments[0].length; i++) {
      (function(url, promise) {
        images[i] = new Image();
        images[i].onload = function() {
          promise.resolve();
        };
        images[i].src = url; 
      })(preload.arguments[0][i], promises[i] = $.Deferred());
      
    }
  }

  var srcList = new Array();
  function loadListener() {
    var lines = this.responseText.split('\n');
    preload(lines);

  }

  function getImages() {

    
    var filename = window.location.href + "images.txt";
    var file = new XMLHttpRequest();
    file.addEventListener("load", loadListener);
    file.open("GET", filename);
    file.send();
  }

  var url = window.location.href;
  if (url.startsWith("http")){
    getImages();
  }

  $.when.apply($, promises).done(function(){
    // put actions after image pre-loading here
    console.log("images finished loading");
  });

  $('link[href$="pubcss-acm-sigchi.css"]').prop('disabled', true);
  $('link[href$="style.css"]').prop('disabled', true);

  // references
  distill_refs = $('dt-bibliography').parent();
  acm_refs = $(distill_refs).clone();
  $(distill_refs).addClass("distill-bib");
  $(acm_refs).addClass("acm-bib");
  ref_header = $(acm_refs).find('h3');
  $(ref_header).replaceWith($('<h2>' + $(ref_header).html() + '</h2>'));
  $('dt-article').append(acm_refs);

  // authors
  distill_authors = $('dt-byline');
  // acm_authors = $('.authors.col-2');
  // $(acm_authors).hide();
  $(acm_refs).hide();
  $('#acm, #terms').hide();

  $('dt-article').prepend($('.title'));
  $('header').hide();


  $('dt-banner').detach();
  // Pace.stop();
  $('#loading').detach();


    if(isSafari){
        $('.authors').addClass('safari');  
    }
    if(isChrome){
        $('.authors').addClass('chrome');  
    }
    if(isFirefox){
         $('.authors').addClass('firefox'); 
         $('figure').addClass('firefox');
    }
  // show ACM layout be default except for the PRIOR example
  if (!url.contains("prior"))
    switchLayout();
  console.log("setup finished.")
});

function afterImagePreload() {
  var loading = document.getElementById("loading_info");
  if (loading != null) loading.style.visibility = "hidden";
  $('.loading_info').hide();
  $(':button').prop('disabled', false);
  $('#loading_info').detach();
  console.log('Preload done');
}

// This function looks for figures with the class '.double-column'
// and moves them to the beginning of the div (used in ACM layout)
function moveFullColumnFiguresToTopOfPage() {
  var figures = $('.double-column');
  if (figures.length){
    // $(figures).addClass("acm");
    $($(figures).get().reverse()).each(function(index){
      $(this).attr("id", "double-column-figure-" + index);
      var parent = $(this).parent();
      var newdiv = document.createElement("figure");
      newdiv.addClass("double-column");
      newdiv.addClass("distill");
      $(newdiv).attr("id", "double-column-figure-" + index);
      $(newdiv).append($(this).children());
      $(parent).prepend(newdiv);
      $(this).hide();
    });

  }
  // counter increment is only necessary if a double column figure is moved before an existing figure
  $('dt-article').css('counter-reset', 'figure');
  // $('dt-article').css('counter-increment', 'figure');
}

// This function moves .double-column figures back to where they
// originally were (used for distill layout)
function moveFullColumnFiguresBack() {
  var figures = $('.double-column.distill');
  if (figures.length){
    $(figures).each(function(index){
      var divid = $(this).attr('id');
      $(this).attr("id", "");
      var origTag = $("#"+divid).last();
      $(origTag).append($(this).children());
      $(origTag).show();
      $(this).detach();
    });

  }
  $('dt-article').css('counter-reset', '');
  $('dt-article').css('counter-increment', '');

}

// function to remove a javascript file
function removejscssfile(filename, filetype){
  var targetelement=(filetype=="js")? "script" : (filetype=="css")? "link" : "none" //determine element type to create nodelist from
  var targetattr=(filetype=="js")? "src" : (filetype=="css")? "href" : "none" //determine corresponding attribute to test for
  var allsuspects=document.getElementsByTagName(targetelement)
  for (var i=allsuspects.length; i>=0; i--){ //search backwards within nodelist for matching elements to remove
  if (allsuspects[i] && allsuspects[i].getAttribute(targetattr)!=null && allsuspects[i].getAttribute(targetattr).indexOf(filename)!=-1)
      allsuspects[i].parentNode.removeChild(allsuspects[i]) //remove element by calling parentNode.removeChild()
  }
}

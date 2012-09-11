
/////////////////////// debug flag ////////////////////////
var debug = false;


/////////////////////// adjustable parameters //////////////////
var minStep = 10;
var nSteps = 30;
var stepInterval = 10;
var blockRange = 15;                    // how far consider one page blocked
var nodeHLColor = 'lightgrey';
var lineHLColor = '#FFFF66';
var lineBlockedColor = '#E9AB17';
var bgColor = '';
var bodyBlockedColor = '#FAF0E6';


///////////////////////// globals ////////////////////////
var eventCount = 0;
var moving = false;
var matchId = -1;
var matchLineId = -1;
var cTimeout;


///////////////////////// utilities ///////////////////////

// No Math.sign() in JS?
function sign(x) {
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    } else {
        return 0;
    }
}


function log(msg) {
    if (debug) {
        console.log("[ " + window.name + " ] " + msg);
    }
}


///////////////////// window scrolling stuff ////////////////////

// accessing other window
if (window.name == 'left') {
    otherside = parent.right;
} else {
    otherside = parent.left;
}

function elementPosition(id) {
    obj = document.getElementById(id);
    var curleft = 0, curtop = 0;

    if (obj && obj.offsetParent) {
        curleft = obj.offsetLeft;
        curtop = obj.offsetTop;

        while (obj = obj.offsetParent) {
            curleft += obj.offsetLeft;
            curtop += obj.offsetTop;
        }
    }

    return { x: curleft, y: curtop };
}


/*
 * Scroll the window to relative position, detecting blocking positions.
 */
function scrollWithBlockCheck(distX, distY) {
    var oldTop = document.body.scrollTop;
    var oldLeft = document.body.scrollLeft;

    eventCount++;
    window.scrollBy(distX, distY);      // the ONLY place for actual scrolling

    var actualX = document.body.scrollLeft - oldLeft;    
    var actualY = document.body.scrollTop - oldTop;
    log("distY=" + distY + ", actualY=" + actualY);

    // extra leewaw here because Chrome scrolling is horribly inacurate
    if ((Math.abs(distX) > blockRange && actualX == 0)
        || Math.abs(distY) > blockRange && actualY == 0) {
        log("blocked");
        eventCount--;
        document.body.style.backgroundColor = bodyBlockedColor;
        putHighlight(matchLineId, lineBlockedColor);
        with (otherside) {
            putHighlight(matchLineId, lineBlockedColor);
        }
        return true;
    } else {
        document.body.style.backgroundColor = bgColor;
        otherside.document.body.style.backgroundColor = bgColor;
        putHighlight(matchLineId, lineHLColor);
        with (otherside) {
            putHighlight(matchLineId, lineHLColor);
        }
        return false;
    }
}


/*
 * timed animation function for scrolling the current window
 */
function matchWindow(n)
{
    moving = true;

    var linkPos = otherside.elementPosition(otherside.matchId).y;
    var linkOffset = linkPos - otherside.document.body.scrollTop;
    var targetPos = document.body.scrollTop + linkOffset
    var curPos = elementPosition(matchId).y;
    var distY = curPos - targetPos;
    var distX = otherside.document.body.scrollLeft
        - document.body.scrollLeft;

    log("matching window... " + n + " distY=" + distY);

    if (distY == 0 && distX == 0) {
        moving = false;
    } else if (n <= 1) {
        scrollWithBlockCheck(distX, distY);
        moving = false;        
    } else{
        var stepSize = Math.floor(Math.abs(distY) / n);
        actualMinStep = Math.min(minStep, Math.abs(distY));
        if (Math.abs(stepSize) < minStep) {
            var step = actualMinStep * sign(distY);
        } else {
            var step = stepSize * sign(distY);
        }
        var blocked = scrollWithBlockCheck(distX, step);
        var rest = Math.floor(distY / step) - 1;
        log("blocked?" + blocked + ", rest steps=" + rest);
        if (!blocked) {
            cTimeout = setTimeout("matchWindow(" + rest + ")", stepInterval);
        } else {
            moving = false;
        }
    }
}


////////////////////////// highlighting /////////////////////////////

function putHighlight(id, color) {
    var elm = document.getElementById(id);
    if (elm != null) {
        elm.style.backgroundColor = color;
    }
}


/*
 * Highlight the link, target nodes and their lines,
 * then start animation to move the other window to match.
 */
function highlight(linkId, targetId, linkLineId, targetLineId)
{
    putHighlight(matchId, bgColor);
    putHighlight(matchLineId, bgColor);
    putHighlight(linkId, nodeHLColor);
    putHighlight(linkLineId, lineHLColor);

    matchId = linkId;
    matchLineId = linkLineId;

    with (otherside) {
        putHighlight(matchId, bgColor);
        putHighlight(matchLineId, bgColor);
        putHighlight(targetId, nodeHLColor);
        putHighlight(targetLineId, lineHLColor);

        matchId = targetId;
        matchLineId = targetLineId;

        cTimeout = setTimeout("matchWindow(" + nSteps + ")" , stepInterval);
    }
}


//////////////////////////// event handling ////////////////////////////
/*
 * Making other side move instantly. Move other side only if:
 * - I am not in an animation initiated by the other side (moving)
 * - I do not have pending program-generated scroll events.
 *
 * Theoretically eventCount alone should work, but this is not the
 * case with Safari, so the 'moving' flag is necessary!
 */
function instantMoveOtherWindow (e) {
    log("moving=" + moving + ", eventCount=" + eventCount);
    if (!moving && eventCount == 0) {
        otherside.matchWindow(1);    
    }
    if (eventCount > 0) {
        eventCount--;
    }
    log("eventCount change=" + eventCount);
}

// Scroll and Resize event handlers
window.onscroll = instantMoveOtherWindow
window.onresize = instantMoveOtherWindow

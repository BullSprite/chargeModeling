var intervalId;
var radius = 2;
var isModelingStarted = false;
var arrInput = [
    [0, 0, 0, 0, 0, 0, 0, 0]
];
var picsArr = [];
var picsTable = document.getElementById("picsTable")
for (var i = 1; i < 25; i++) {
    picsArr.push(document.getElementById("p" + i))
    console.log("p: " + i +" "+ picsArr[i-1].src)
}

var state = "requesting";
var currentIntervalID = 1100223366554866;
var downloadTag = document.getElementById("downloadA")
var isTraceVisible = true;

var submitButton = document.getElementById("submitButton");
var context1, context2, canvas1, canvas2;
var toggleTraceShifting = document.getElementById("toggleTraceShifting"),
    iterator = 0;


var WIDTH, HEIGHT;
var obj1X = document.getElementById("xCoordFirst"),
    obj1Y = document.getElementById("yCoordFirst"),
    obj2X = document.getElementById("xCoordSecond"),
    obj2Y = document.getElementById("yCoordSecond"),
    obj3X = document.getElementById("xCoordThird"),
    obj3Y = document.getElementById("yCoordThird"),
    charge1 = document.getElementById("chargeFirst"),
    charge2 = document.getElementById("chargeSecond"),
    charge3 = document.getElementById("chargeThird"),
    chargeS = document.getElementById("chargeShifting"),
    sObjX = document.getElementById("xCoordShifting"),
    sObjY = document.getElementById("yCoordShifting"),
    myform,
    output,
    d = document;

canvas1 = document.getElementById("canvas1");
canvas2 = document.getElementById("canvas2");
canvas3 = document.getElementById("canvas3");
WIDTH = canvas1.width;
HEIGHT = canvas1.height;
context1 = canvas1.getContext("2d");
context2 = canvas2.getContext("2d");
context3 = canvas3.getContext("2d");
var scale = 3;
context2.scale(scale, scale);
context1.scale(scale, scale);
//context3.scale(scale,scale);
context3.beginPath();
context3.moveTo(WIDTH/2,0);
context3.lineTo(WIDTH/2,HEIGHT);
context3.stroke();

context3.beginPath();
context3.moveTo(0,HEIGHT/2);
context3.lineTo(WIDTH,HEIGHT/2);
context3.stroke();

class Charge {
    constructor(chargeInCoulomb, ctx1, ctx2, radius) {
        this.chargeInCoulomb = chargeInCoulomb;
        this.firstLayerContext = ctx1;
        this.secondLayerContext = ctx2;
        this.traceArray = [];
        this.radius = radius;
        this.color = "rgb(0,0,0)";
    }

    determineColor(chargeInCoulomb, chargeObj) {
        if (chargeObj.chargeInCoulomb > 0) {
            chargeObj.color = "rgb(0," + (200 - (chargeObj.chargeInCoulomb * 15)) + ",255)";
        } else if (chargeObj.chargeInCoulomb < 0) {
            chargeObj.color = "rgb(255,0," + (68 - (chargeObj.chargeInCoulomb * 15)) + ")";
        }
        if (chargeObj.chargeInCoulomb >= 10) {
            chargeObj.color = "rgb(0,0,255)";
        } else if (chargeObj.chargeInCoulomb <= -10) {
            chargeObj.color = "rgb(255,0,50)";
        }
        if (chargeObj.chargeInCoulomb == 0) {
            chargeObj.color = "#ffe9f1"
        }
        return chargeObj.color;
    }

    drawCharge(x, y, chargeObj) {
        chargeObj.secondLayerContext.beginPath();
        chargeObj.secondLayerContext.lineWidth = 1;
        chargeObj.secondLayerContext.arc(x, y, chargeObj.radius, 0, Math.PI * 2 * chargeObj.radius);
        chargeObj.secondLayerContext.fillStyle = chargeObj.determineColor(chargeObj.chargeInCoulomb, chargeObj);
        chargeObj.secondLayerContext.fill();
        chargeObj.secondLayerContext.stroke();
        chargeObj.secondLayerContext.beginPath();
        chargeObj.secondLayerContext.lineWidth = 1;
        chargeObj.secondLayerContext.arc(x, y, chargeObj.radius, 0, Math.PI * 2 * chargeObj.radius);
        chargeObj.secondLayerContext.stroke();
    }

    drawMotionWithTrace(fromX, fromY, toX, toY, chargeObj) {
        chargeObj.firstLayerContext.beginPath();
        chargeObj.firstLayerContext.moveTo(fromX, fromY);
        chargeObj.firstLayerContext.lineTo(toX, toY);
        chargeObj.traceArray.push([toX, toY]);
        chargeObj.drawCharge(toX, toY, chargeObj);
        chargeObj.firstLayerContext.stroke();

    }

    drawMotion(toX, toY, chargeObj) {
        chargeObj.secondLayerContext.beginPath();
        chargeObj.drawCharge(toX, toY, chargeObj);
        chargeObj.traceArray.push([toX, toY]);
        chargeObj.secondLayerContext.stroke();
    }

    clearTraceArr(chargeObj) {
        while (chargeObj.traceArray.length != 0)
            chargeObj.traceArray.pop();
    }

    drawTraceFrom(arrayCellNumber, chargeObj) {
        chargeObj.firstLayerContext.beginPath();
        chargeObj.firstLayerContext.moveTo(chargeObj.traceArray[0][0], chargeObj.traceArray[0][1]);
        for (var i = arrayCellNumber; i < chargeObj.traceArray.length; i++) {
            chargeObj.firstLayerContext.lineTo(chargeObj.traceArray[i][0], chargeObj.traceArray[i][1]);
            chargeObj.firstLayerContext.stroke();
        }
    }
}
var isTranslated = false;
class DrawingUtils {
    constructor(shiftingCharge, charge1, charge2, charge3, firstLayerContext, secondLayerContext, w, h, drawingArray) {
        this.shiftingCharge = shiftingCharge;
        this.charge1 = charge1;
        this.charge2 = charge2;
        this.charge3 = charge3;
        this.intervalId = 0;
        this.drawingArray = drawingArray;
        this.globalIterator = 0;
        this.redrawTime = 10;
        this.firstLayerContext = firstLayerContext;
        this.secondLayerContext = secondLayerContext;
        this.canvasWidthNHeight = [w, h];
    }

    clearTrace(utilsObj) {
        if (isTraceVisible) {
            canvas1.style.visibility = "hidden";
            toggleTraceShifting.value = "показать следы";
            isTraceVisible = false;
        } else {
            canvas1.style.visibility = "visible";
            toggleTraceShifting.value = "скрыть следы";
            isTraceVisible = true;
        }

    }


    draw(utilsObj) {
        iterator = 0;
        clearInterval(currentIntervalID);
        console.log("interval cleared");
        if (isTranslated) {

            console.log("isTranslated true")
            context1.translate(-WIDTH/6,-HEIGHT/6);
            context1.clearRect(0, 0, 1000, 1000);
            context1.translate(WIDTH/6,HEIGHT/6);

        } else {
            context1.translate(WIDTH/6,HEIGHT/6);
            context2.translate(WIDTH/6,HEIGHT/6);
            isTranslated = true;
            console.log("translate");
        }


        currentIntervalID = setInterval(function() {
            context2.translate(-WIDTH/6,-HEIGHT/6);
            context2.clearRect(0, 0, 1000, 1000);
            context2.translate(WIDTH/6,HEIGHT/6);
            console.log("asdf");
            if (iterator == 0) {
                utilsObj.shiftingCharge.drawMotionWithTrace(
                    utilsObj.drawingArray[iterator][0],
                    utilsObj.drawingArray[iterator][1],
                    utilsObj.drawingArray[iterator][0],
                    utilsObj.drawingArray[iterator][1],
                    utilsObj.shiftingCharge
                );
                utilsObj.charge1.drawMotionWithTrace(
                    utilsObj.drawingArray[iterator][2],
                    utilsObj.drawingArray[iterator][3],
                    utilsObj.drawingArray[iterator][2],
                    utilsObj.drawingArray[iterator][3],
                    utilsObj.charge1
                );
                utilsObj.charge2.drawMotionWithTrace(
                    utilsObj.drawingArray[iterator][4],
                    utilsObj.drawingArray[iterator][5],
                    utilsObj.drawingArray[iterator][4],
                    utilsObj.drawingArray[iterator][5],
                    utilsObj.charge2
                );
                utilsObj.charge3.drawMotionWithTrace(
                    utilsObj.drawingArray[iterator][6],
                    utilsObj.drawingArray[iterator][7],
                    utilsObj.drawingArray[iterator][6],
                    utilsObj.drawingArray[iterator][7],
                    utilsObj.charge3
                );
            } else {
                if (utilsObj.drawingArray[iterator] != undefined) {
                    utilsObj.shiftingCharge.drawMotionWithTrace(
                        utilsObj.drawingArray[iterator - 1][0],
                        utilsObj.drawingArray[iterator - 1][1],
                        utilsObj.drawingArray[iterator][0],
                        utilsObj.drawingArray[iterator][1],
                        utilsObj.shiftingCharge
                    );
                    utilsObj.charge1.drawMotionWithTrace(
                        utilsObj.drawingArray[iterator - 1][2],
                        utilsObj.drawingArray[iterator - 1][3],
                        utilsObj.drawingArray[iterator][2],
                        utilsObj.drawingArray[iterator][3],
                        utilsObj.charge1
                    );
                    utilsObj.charge2.drawMotionWithTrace(
                        utilsObj.drawingArray[iterator - 1][4],
                        utilsObj.drawingArray[iterator - 1][5],
                        utilsObj.drawingArray[iterator][4],
                        utilsObj.drawingArray[iterator][5],
                        utilsObj.charge2
                    );
                    utilsObj.charge3.drawMotionWithTrace(
                        utilsObj.drawingArray[iterator - 1][6],
                        utilsObj.drawingArray[iterator - 1][7],
                        utilsObj.drawingArray[iterator][6],
                        utilsObj.drawingArray[iterator][7],
                        utilsObj.charge3
                    );
                } else {
                    //alert(iterator);
                }


            }
            if (iterator == utilsObj.drawingArray.length - 1) {
                clearInterval(currentIntervalID);
            }
            iterator += 1;
        }, utilsObj.redrawTime);
        console.log(currentIntervalID);
    }


    stopDrawing(intervalId) {
        clearInterval(intervalId);
    }


}


var shiftingCharge, chargeFirst, chargeSecond, chargeThird, utils;


submitButton.onclick = function() {



}
toggleTraceShifting.onclick = function() {
    if (isTraceVisible) {
        canvas1.style.visibility = "hidden";
        toggleTraceShifting.value = "показать следы";
        isTraceVisible = false;
    } else {
        canvas1.style.visibility = "visible";
        toggleTraceShifting.value = "скрыть следы";
        isTraceVisible = true;
    }
}

var form = document.getElementById("submitData");
var request = new XMLHttpRequest();
var drawArr = [];
//request.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
//request.setRequestHeader()
form.addEventListener('submit', function(evt) {
    evt.preventDefault();
    if (state == "modeling") {
        console.log(utils);
        clearInterval(currentIntervalID);
        console.log(state + " draw");
        var d = new Date()
        var tempStr = downloadTag.href.split("?")[0];
        tempStr += "?" + d.getTime;
        downloadTag.href = tempStr;
        for (var i = 0 ;i < picsArr.length; i++) {
            //console.log('?' + new Date() + "??????????????????")
            
            var s = picsArr[i].src.split("?")[0];
            picsArr[i].src  =s +"?"+d.getTime();
            console.log(picsArr[i].src.split("?"))
        }
        console.log('?' + new Date() + "??????????????????")
        utils.draw(utils);
        state = "requesting";
        submitButton.disabled = false;
        submitButton.value = "задать параметры системы";
    } else if (state == "requesting") {
        //var request = new XMLHttpRequest();
        request.open('POST', 'server.php', true);


        var formData = new FormData(form);
        console.log(formData);

        request.send(formData);
        submitButton.disabled = true;
        submitButton.value = "идет рассчет траектории зарядов, подождите";

        request.onreadystatechange = function() {

            if (request.readyState === 4) {
                if (request.status == 200 && request.status < 300) {
                    var arrR = request.response.split(":");
                    drawArr = [];
                    for (var i = 0; i < arrR.length; i++) {
                        drawArr.push(arrR[i].split(','));
                    }
                    shiftingCharge = new Charge(chargeShifting.value, context1, context2, radius);
                    chargeFirst = new Charge(charge1.value, context1, context2, radius);
                    chargeSecond = new Charge(charge2.value, context1, context2, radius);
                    chargeThird = new Charge(charge3.value, context1, context2, radius);

                    utils = new DrawingUtils(shiftingCharge, chargeFirst, chargeSecond, chargeThird, context1, context2, WIDTH, HEIGHT, drawArr);
                    submitButton.disabled = false;
                    submitButton.value = "траектория просчитана, начать моделирование";
                    state = "modeling";

                    //utils.draw(utils);
                    console.log(drawArr);
                }

            }
        }
    }


});
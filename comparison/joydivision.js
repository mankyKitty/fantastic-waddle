window.onload = function () {
  var canvas = document.querySelector('canvas');
  var context = canvas.getContext('2d');

  var size = 300; // window.innerWidth;

  canvas.width = 320; // size;
  canvas.height = 320; //size;
  context.fillStyle = '#f9f9f9';
  context.lineWidth = 2;

  var step = 10;
  var lines = [];

  // Create the lines
  for (var i = step; i <= size - step; i += step) {

    var line = [];
    for( var j = step; j <= size - step; j+= step ) {
      var distanceToCenter = Math.abs(j - size / 2);
      var variance = Math.max(size / 2 - 50 - distanceToCenter, 0);
      var random = Math.random() * variance / 2 * -1;
      var point = {x: j, y: i + random};
      line.push(point);
    }
    lines.push(line);
  }

  // Do the drawing
  for (var i = 0; i < lines.length; i++) {

    context.beginPath();
    context.moveTo(lines[i][0].x, lines[i][0].y);
    for( var j = 0; j < lines[i].length - 2; j++) {
      var xc = (lines[i][j].x + lines[i][j + 1].x) / 2;
      var yc = (lines[i][j].y + lines[i][j + 1].y) / 2;
      console.log("Control Points");
      console.log(xc, yc);
      context.quadraticCurveTo(lines[i][j].x, lines[i][j].y, xc, yc);
    }

    context.quadraticCurveTo(lines[i][j].x, lines[i][j].y, lines[i][j + 1].x, lines[i][j + 1].y);
    context.fill();

    context.stroke();
  }
};

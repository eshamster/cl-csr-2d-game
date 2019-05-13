var protoClClientSideRendering_client_graphics = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  function pushVerticesTo(geometry, rawVertexLst) {
      for (var vertexAsLst = null, _js_idx103 = 0; _js_idx103 < rawVertexLst.length; _js_idx103 += 1) {
          vertexAsLst = rawVertexLst[_js_idx103];
          geometry.vertices.push(new THREE.Vector3(vertexAsLst[0], vertexAsLst[1], 0));
      };
  };
  function pushFacesTo(geometry, rawFaceLst) {
      for (var faceAsLst = null, _js_idx104 = 0; _js_idx104 < rawFaceLst.length; _js_idx104 += 1) {
          faceAsLst = rawFaceLst[_js_idx104];
          geometry.faces.push(new THREE.Face3(faceAsLst[0], faceAsLst[1], faceAsLst[2]));
      };
  };
  function toRad(degree) {
      return (degree * Math.PI) / 180;
  };
  function makeLineModel(geometry, color) {
      var material = new THREE.LineBasicMaterial({ 'color' : color });
      __PS_MV_REG = [];
      return new THREE.Line(geometry, material);
  };
  function makeSolidModel(geometry, color) {
      var material = new THREE.MeshBasicMaterial({ 'color' : color });
      __PS_MV_REG = [];
      return new THREE.Mesh(geometry, material);
  };
  function getMeshWidth(mesh) {
      return mesh.geometry.boundingBox.max.x - mesh.geometry.boundingBox.min.x;
  };
  function getMeshHeight(mesh) {
      return mesh.geometry.boundingBox.max.y - mesh.geometry.boundingBox.min.y;
  };
  function getMeshSize(mesh) {
      __PS_MV_REG = [];
      return ['width', getMeshWidth(mesh), 'height', getMeshHeight(mesh)];
  };
  function makeLine() {
      var _js106 = arguments.length;
      for (var n105 = 0; n105 < _js106; n105 += 2) {
          switch (arguments[n105]) {
          case 'pos-a':
              posA = arguments[n105 + 1];
              break;
          case 'pos-b':
              posB = arguments[n105 + 1];
              break;
          case 'color':
              color = arguments[n105 + 1];
          };
      };
      var posA;
      var posB;
      var color;
      var g198927 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198927, rest);
      };
      pushVertices([posA[0], posA[1]], [posB[0], posB[1]]);
      __PS_MV_REG = [];
      return makeLineModel(g198927, color);
  };
  function makeLines() {
      var _js108 = arguments.length;
      for (var n107 = 0; n107 < _js108; n107 += 2) {
          switch (arguments[n107]) {
          case 'pnt-list':
              pntList = arguments[n107 + 1];
              break;
          case 'color':
              color = arguments[n107 + 1];
          };
      };
      var pntList;
      var color;
      var g198931 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198931, rest);
      };
      for (var pnt = null, _js_idx109 = 0; _js_idx109 < pntList.length; _js_idx109 += 1) {
          pnt = pntList[_js_idx109];
          pushVertices([pnt[0], pnt[1]]);
      };
      __PS_MV_REG = [];
      return makeLineModel(g198931, color);
  };
  function makeSolidRect() {
      var _js111 = arguments.length;
      for (var n110 = 0; n110 < _js111; n110 += 2) {
          switch (arguments[n110]) {
          case 'width':
              width = arguments[n110 + 1];
              break;
          case 'height':
              height = arguments[n110 + 1];
              break;
          case 'color':
              color = arguments[n110 + 1];
          };
      };
      var width;
      var height;
      var color;
      var g198941 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198941, rest);
      };
      var pushFaces = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushFacesTo(g198941, rest);
      };
      pushVertices([0, 0], [width, 0], [width, height], [0, height]);
      pushFaces([0, 1, 2], [2, 3, 0]);
      __PS_MV_REG = [];
      return makeSolidModel(g198941, color);
  };
  function makeWiredRect() {
      var _js113 = arguments.length;
      for (var n112 = 0; n112 < _js113; n112 += 2) {
          switch (arguments[n112]) {
          case 'width':
              width = arguments[n112 + 1];
              break;
          case 'height':
              height = arguments[n112 + 1];
              break;
          case 'color':
              color = arguments[n112 + 1];
          };
      };
      var width;
      var height;
      var color;
      var g198951 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198951, rest);
      };
      pushVertices([0, 0], [width, 0], [width, height], [0, height], [0, 0]);
      __PS_MV_REG = [];
      return makeLineModel(g198951, color);
  };
  function makeSolidRegularPolygon() {
      var _js115 = arguments.length;
      for (var n114 = 0; n114 < _js115; n114 += 2) {
          switch (arguments[n114]) {
          case 'r':
              r = arguments[n114 + 1];
              break;
          case 'n':
              n = arguments[n114 + 1];
              break;
          case 'start-angle':
              startAngle = arguments[n114 + 1];
              break;
          case 'color':
              color = arguments[n114 + 1];
          };
      };
      var r;
      var n;
      var startAngle = 'undefined' === typeof startAngle ? 0 : startAngle;
      var color;
      var g198961 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198961, rest);
      };
      var pushFaces = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushFacesTo(g198961, rest);
      };
      for (var i = 0; i < n; i += 1) {
          var angle = toRad((360 * i) / n + startAngle);
          pushVertices([r * Math.cos(angle), r * Math.sin(angle)]);
      };
      pushVertices([0, 0]);
      for (var i = 0; i < n; i += 1) {
          pushFaces([n, i, (i + 1) % n]);
      };
      __PS_MV_REG = [];
      return makeSolidModel(g198961, color);
  };
  function makeWiredRegularPolygon() {
      var _js117 = arguments.length;
      for (var n116 = 0; n116 < _js117; n116 += 2) {
          switch (arguments[n116]) {
          case 'r':
              r = arguments[n116 + 1];
              break;
          case 'n':
              n = arguments[n116 + 1];
              break;
          case 'start-angle':
              startAngle = arguments[n116 + 1];
              break;
          case 'color':
              color = arguments[n116 + 1];
          };
      };
      var r;
      var n;
      var startAngle = 'undefined' === typeof startAngle ? 0 : startAngle;
      var color;
      var g198971 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g198971, rest);
      };
      for (var i = 0; i < n + 1; i += 1) {
          var angle = toRad((360 * i) / n + startAngle);
          pushVertices([r * Math.cos(angle), r * Math.sin(angle)]);
      };
      __PS_MV_REG = [];
      return makeLineModel(g198971, color);
  };
  function makeSolidCircle() {
      var _js119 = arguments.length;
      for (var n118 = 0; n118 < _js119; n118 += 2) {
          switch (arguments[n118]) {
          case 'r':
              r = arguments[n118 + 1];
              break;
          case 'color':
              color = arguments[n118 + 1];
          };
      };
      var r;
      var color;
      __PS_MV_REG = [];
      return makeSolidRegularPolygon('r', r, 'n', 60, 'color', color);
  };
  function makeWiredCircle() {
      var _js121 = arguments.length;
      for (var n120 = 0; n120 < _js121; n120 += 2) {
          switch (arguments[n120]) {
          case 'r':
              r = arguments[n120 + 1];
              break;
          case 'color':
              color = arguments[n120 + 1];
          };
      };
      var r;
      var color;
      __PS_MV_REG = [];
      return makeWiredRegularPolygon('r', r, 'n', 60, 'color', color);
  };
  function makeWiredPolygon() {
      var _js123 = arguments.length;
      for (var n122 = 0; n122 < _js123; n122 += 2) {
          switch (arguments[n122]) {
          case 'pnt-list':
              pntList = arguments[n122 + 1];
              break;
          case 'color':
              color = arguments[n122 + 1];
          };
      };
      var pntList;
      var color;
      var g199005 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g199005, rest);
      };
      for (var pnt = null, _js_idx124 = 0; _js_idx124 < pntList.length; _js_idx124 += 1) {
          pnt = pntList[_js_idx124];
          pushVertices(pnt);
      };
      pushVertices(pntList[0]);
      __PS_MV_REG = [];
      return makeLineModel(g199005, color);
  };
  function makeSolidPolygon() {
      var _js126 = arguments.length;
      for (var n125 = 0; n125 < _js126; n125 += 2) {
          switch (arguments[n125]) {
          case 'pnt-list':
              pntList = arguments[n125 + 1];
              break;
          case 'color':
              color = arguments[n125 + 1];
          };
      };
      var pntList;
      var color;
      var g199009 = new THREE.Geometry();
      var pushVertices = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushVerticesTo(g199009, rest);
      };
      var pushFaces = function () {
          var rest = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return pushFacesTo(g199009, rest);
      };
      for (var pnt = null, _js_idx127 = 0; _js_idx127 < pntList.length; _js_idx127 += 1) {
          pnt = pntList[_js_idx127];
          pushVertices(pnt);
      };
      var len = pntList.length;
      for (var i = 0; i < len - 1; i += 1) {
          pushFaces([0, i + 1, (i + 2) % len]);
      };
      __PS_MV_REG = [];
      return makeSolidModel(g199009, color);
  };
  function changeModelColor(model2d, newColorRgb) {
      model2d.model.material.color = new THREE.Color(newColorRgb);
      model2d.model.material.needsUpdate = true;
      __PS_MV_REG = [];
      return model2d;
  };
  /* --- extern symbols --- */
  return {
    'getMeshWidth': getMeshWidth,
    'getMeshHeight': getMeshHeight,
    'getMeshSize': getMeshSize,
    'makeLine': makeLine,
    'makeLines': makeLines,
    'makeSolidRect': makeSolidRect,
    'makeWiredRect': makeWiredRect,
    'makeSolidRegularPolygon': makeSolidRegularPolygon,
    'makeWiredRegularPolygon': makeWiredRegularPolygon,
    'makeSolidCircle': makeSolidCircle,
    'makeWiredCircle': makeWiredCircle,
    'makeWiredPolygon': makeWiredPolygon,
    'makeSolidPolygon': makeSolidPolygon,
    'changeModelColor': changeModelColor,
    '_internal': {
      'pushVerticesTo': pushVerticesTo,
      'pushFacesTo': pushFacesTo,
      'toRad': toRad,
      'makeLineModel': makeLineModel,
      'makeSolidModel': makeSolidModel,
    }
  };
})();

var protoClClientSideRendering_protocol = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  if ('undefined' === typeof CODETONAMETABLE) {
      var CODETONAMETABLE = null;
  };
  if ('undefined' === typeof NAMETOCODETABLE) {
      var NAMETOCODETABLE = null;
  };
  function initializeTable() {
      CODETONAMETABLE = {  };
      NAMETOCODETABLE = {  };
      for (var pair = null, _js_arrvar129 = [[0, 'frame-start'], [1, 'frame-end'], [10, 'delete-draw-object'], [11, 'draw-rect'], [12, 'draw-circle'], [21, 'log-console'], [-1, 'key-down'], [-2, 'key-up'], [-11, 'mouse-down'], [-12, 'mouse-up'], [-13, 'mouse-move']], _js_idx128 = 0; _js_idx128 < _js_arrvar129.length; _js_idx128 += 1) {
          pair = _js_arrvar129[_js_idx128];
          var code = pair[0];
          var name = pair[1];
          CODETONAMETABLE[code] = name;
          NAMETOCODETABLE[name] = code;
      };
  };
  initializeTable();
  function codeToName(code) {
      return CODETONAMETABLE[code];
  };
  function nameToCode(name) {
      return NAMETOCODETABLE[name];
  };
  function drawCodeP(code) {
      var targetName = codeToName(code);
      __PS_MV_REG = [];
      return ['delete-draw-object', 'draw-rect', 'draw-circle'].some(function (name) {
          return name === targetName;
      });
  };
  function boolToNumber(bool) {
      return bool ? 1 : 0;
  };
  function numberToBool(number) {
      return number === 1 ? true : null;
  };
  /* --- extern symbols --- */
  return {
    'codeToName': codeToName,
    'nameToCode': nameToCode,
    'drawCodeP': drawCodeP,
    'boolToNumber': boolToNumber,
    'numberToBool': numberToBool,
    '_internal': {
      'CODETONAMETABLE': CODETONAMETABLE,
      'NAMETOCODETABLE': NAMETOCODETABLE,
      'initializeTable': initializeTable,
    }
  };
})();

var protoClClientSideRendering_client_message = (function() {
  /* --- import symbols --- */
  var nameToCode = protoClClientSideRendering_protocol.nameToCode;
  var makeSolidRect = protoClClientSideRendering_client_graphics.makeSolidRect;
  var numberToBool = protoClClientSideRendering_protocol.numberToBool;
  var drawCodeP = protoClClientSideRendering_protocol.drawCodeP;
  var makeSolidCircle = protoClClientSideRendering_client_graphics.makeSolidCircle;
  var makeWiredCircle = protoClClientSideRendering_client_graphics.makeWiredCircle;
  var codeToName = protoClClientSideRendering_protocol.codeToName;
  var makeWiredRect = protoClClientSideRendering_client_graphics.makeWiredRect;
  /* --- define objects --- */
  if ('undefined' === typeof FRAMEJSONBUFFER) {
      var FRAMEJSONBUFFER = [];
  };
  if ('undefined' === typeof DRAWCOMMANDBUFFER) {
      var DRAWCOMMANDBUFFER = [];
  };
  if ('undefined' === typeof DRAWCOMMANDQUEUE) {
      var DRAWCOMMANDQUEUE = [];
  };
  function pushDrawCommandToBuffer(parsedMessage) {
      __PS_MV_REG = [];
      return DRAWCOMMANDBUFFER.push(parsedMessage);
  };
  function queueDrawCommandsInBuffer() {
      DRAWCOMMANDQUEUE.unshift(DRAWCOMMANDBUFFER);
      __PS_MV_REG = [];
      return DRAWCOMMANDBUFFER = [];
  };
  function dequeueDrawCommands() {
      __PS_MV_REG = [];
      return DRAWCOMMANDQUEUE.pop();
  };
  function pushMessageToBuffer(parsedMessage) {
      __PS_MV_REG = [];
      return FRAMEJSONBUFFER.push(parsedMessage);
  };
  function printMessageStat(messageStat) {
      var gSequence132;
      var gKey133;
      var total = 0;
      var text = '';
      for (var key = null, _js_arrvar131 = (gSequence132 = Object.keys(messageStat), gKey133 = null, (gSequence132.sort(function (a, b) {
          var keyA = a;
          var keyB = b;
          return (function (a, b) {
              return a < b;
          })(keyA, keyB) ? -1 : 1;
      }), gSequence132)), _js_idx130 = 0; _js_idx130 < _js_arrvar131.length; _js_idx130 += 1) {
          key = _js_arrvar131[_js_idx130];
          var count = messageStat[key];
          text += codeToName(key) + ':' + '\t' + count + '\n';
          total += count;
      };
      text = 'TOTAL: ' + total + '\n' + '---' + '\n' + text;
      __PS_MV_REG = [];
      return document.getElementById('js-code').value = text;
  };
  function processMessage(message) {
      var parsedMessage = receivingToJson(message);
      pushMessageToBuffer(parsedMessage);
      if (targetKindP('frame-end', parsedMessage)) {
          var messageStat = {  };
          for (var parsed = null, _js_idx134 = 0; _js_idx134 < FRAMEJSONBUFFER.length; _js_idx134 += 1) {
              parsed = FRAMEJSONBUFFER[_js_idx134];
              var kindCode = parsed['kind'];
              if (!messageStat[kindCode]) {
                  messageStat[kindCode] = 0;
              };
              ++messageStat[kindCode];
              if (codeToName(kindCode) === 'log-console') {
                  interpretLogConsole(parsed);
              } else if (drawCodeP(kindCode)) {
                  pushDrawCommandToBuffer(parsed);
              };
          };
          printMessageStat(messageStat);
          queueDrawCommandsInBuffer();
          __PS_MV_REG = [];
          return FRAMEJSONBUFFER = [];
      };
  };
  function targetKindP(kind, parsedMessage) {
      __PS_MV_REG = [];
      return parsedMessage['kind'] === nameToCode(kind);
  };
  function receivingToJson(message) {
      __PS_MV_REG = [];
      return JSON.parse(message);
  };
  function interpretLogConsole(command) {
      __PS_MV_REG = [];
      return console.log(command['data']['message']);
  };
  function drawInfo() {
      this.kind = null;
      this.data = null;
      this.mesh = null;
      return this;
  };
  function makeDrawInfo() {
      var _js136 = arguments.length;
      for (var n135 = 0; n135 < _js136; n135 += 2) {
          switch (arguments[n135]) {
          case 'kind':
              kind = arguments[n135 + 1];
              break;
          case 'data':
              data = arguments[n135 + 1];
              break;
          case 'mesh':
              mesh = arguments[n135 + 1];
          };
      };
      var kind;
      var data;
      var mesh;
      var result = new drawInfo();
      result.kind = kind;
      result.data = data;
      result.mesh = mesh;
      __PS_MV_REG = [];
      return result;
  };
  function drawInfoP(obj) {
      return (obj instanceof drawInfo);
  };
  if ('undefined' === typeof DRAWINFOTABLE) {
      var DRAWINFOTABLE = {  };
  };
  function updateCommonMeshParams(mesh, dataTable) {
      mesh.position.set(dataTable['x'], dataTable['y'], dataTable['depth']);
      var rotate = dataTable['rotate'];
      __PS_MV_REG = [];
      return rotate ? (mesh.rotation.z = rotate) : null;
  };
  function makeMeshByCommand(command) {
      var kind = codeToName(command['kind']);
      var data = command['data'];
      var mesh = (function () {
          switch (kind) {
          case 'draw-circle':
              __PS_MV_REG = [];
              return numberToBool(data['fill-p']) ? makeSolidCircle('r', data['r'], 'color', data['color']) : makeWiredCircle('r', data['r'], 'color', data['color']);
          case 'draw-rect':
              __PS_MV_REG = [];
              return numberToBool(data['fill-p']) ? makeSolidRect('width', data['width'], 'height', data['height'], 'color', data['color']) : makeWiredRect('width', data['width'], 'height', data['height'], 'color', data['color']);
          default:
              throw 'Message: ' + 'The value ~A is not of the expected type (MEMBER ~A)' + '; Args: ' + kind + ', ' + 'draw-circle'('draw-rect');
          };
      })();
      updateCommonMeshParams(mesh, data);
      __PS_MV_REG = [];
      return mesh;
  };
  function addMeshToScene(scene, mesh) {
      __PS_MV_REG = [];
      return scene.add(mesh);
  };
  function removeMeshFromScene(scene, mesh) {
      __PS_MV_REG = [];
      return scene.remove(mesh);
  };
  function shouldRecreateP(prevInfo, newKind, newData) {
      var prevKind = prevInfo.kind;
      var prevData = prevInfo.data;
      var eqParams = function () {
          var keyList = Array.prototype.slice.call(arguments, 0);
          __PS_MV_REG = [];
          return keyList.every(function (key) {
              return prevData[key] === newData[key];
          });
      };
      __PS_MV_REG = [];
      return prevKind !== newKind || (function () {
          switch (newKind) {
          case 'draw-circle':
              __PS_MV_REG = [];
              return !eqParams('fill-p', 'color', 'r');
          case 'draw-rect':
              __PS_MV_REG = [];
              return !eqParams('fill-p', 'color', 'width', 'height');
          default:
              __PS_MV_REG = [];
              return true;
          };
      })();
  };
  function addOrUpdateMesh(scene, command) {
      var kind = codeToName(command['kind']);
      var data = command['data'];
      var id = data['id'];
      var prevInfo = DRAWINFOTABLE[id];
      if (kind === 'delete-draw-object') {
          var result = id in DRAWINFOTABLE;
          delete DRAWINFOTABLE[id];
          result;
          __PS_MV_REG = [];
          return removeMeshFromScene(scene, prevInfo.mesh);
      } else if (prevInfo == null) {
          var mesh = makeMeshByCommand(command);
          DRAWINFOTABLE[id] = makeDrawInfo('kind', kind, 'data', data, 'mesh', mesh);
          __PS_MV_REG = [];
          return addMeshToScene(scene, mesh);
      } else if (shouldRecreateP(prevInfo, kind, data)) {
          var result137 = id in DRAWINFOTABLE;
          delete DRAWINFOTABLE[id];
          result137;
          removeMeshFromScene(scene, prevInfo.mesh);
          __PS_MV_REG = [];
          return addOrUpdateMesh(scene, command);
      } else {
          updateCommonMeshParams(prevInfo.mesh, data);
          __PS_MV_REG = [];
          return prevInfo.data = data;
      };
  };
  function interpretDrawCommand(scene, command) {
      __PS_MV_REG = [];
      return addOrUpdateMesh(scene, command);
  };
  /* --- extern symbols --- */
  return {
    'dequeueDrawCommands': dequeueDrawCommands,
    'processMessage': processMessage,
    'interpretDrawCommand': interpretDrawCommand,
    '_internal': {
      'FRAMEJSONBUFFER': FRAMEJSONBUFFER,
      'DRAWCOMMANDBUFFER': DRAWCOMMANDBUFFER,
      'DRAWCOMMANDQUEUE': DRAWCOMMANDQUEUE,
      'pushDrawCommandToBuffer': pushDrawCommandToBuffer,
      'queueDrawCommandsInBuffer': queueDrawCommandsInBuffer,
      'pushMessageToBuffer': pushMessageToBuffer,
      'printMessageStat': printMessageStat,
      'targetKindP': targetKindP,
      'receivingToJson': receivingToJson,
      'interpretLogConsole': interpretLogConsole,
      'drawInfo': drawInfo,
      'makeDrawInfo': makeDrawInfo,
      'drawInfoP': drawInfoP,
      'DRAWINFOTABLE': DRAWINFOTABLE,
      'updateCommonMeshParams': updateCommonMeshParams,
      'makeMeshByCommand': makeMeshByCommand,
      'addMeshToScene': addMeshToScene,
      'removeMeshFromScene': removeMeshFromScene,
      'shouldRecreateP': shouldRecreateP,
      'addOrUpdateMesh': addOrUpdateMesh,
    }
  };
})();

var protoClClientSideRendering_client_socket = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  if ('undefined' === typeof WSSOCKET) {
      var WSSOCKET = new WebSocket('ws://' + window.location.host + '/ws');
  };
  function registerSocketOnMessage(callback) {
      return WSSOCKET.onmessage = function (e) {
          return callback(e.data);
      };
  };
  function sendJsonToServer(json) {
      __PS_MV_REG = [];
      return WSSOCKET.send(JSON.stringify(json));
  };
  /* --- extern symbols --- */
  return {
    'registerSocketOnMessage': registerSocketOnMessage,
    'sendJsonToServer': sendJsonToServer,
    '_internal': {
      'WSSOCKET': WSSOCKET,
    }
  };
})();

var protoClClientSideRendering_client_global = (function() {
  /* --- import symbols --- */

  /* --- define objects --- */
  if ('undefined' === typeof RENDEREDDOM) {
      var RENDEREDDOM = null;
  };
  function getRenderedDom() {
      return RENDEREDDOM;
  };
  function setRenderedDom(dom) {
      return RENDEREDDOM = dom;
  };
  if ('undefined' === typeof SCREENWIDTH) {
      var SCREENWIDTH = null;
  };
  if ('undefined' === typeof SCREENHEIGHT) {
      var SCREENHEIGHT = null;
  };
  if ('undefined' === typeof SCREENSCALE) {
      var SCREENSCALE = 1;
  };
  function getScreenWidth() {
      return SCREENWIDTH;
  };
  function getScreenHeight() {
      return SCREENHEIGHT;
  };
  function getScreenScale() {
      return SCREENSCALE;
  };
  function setScreenSize(width, height, scale) {
      SCREENWIDTH = width;
      SCREENHEIGHT = height;
      return SCREENSCALE = scale;
  };
  /* --- extern symbols --- */
  return {
    'getRenderedDom': getRenderedDom,
    'setRenderedDom': setRenderedDom,
    'getScreenWidth': getScreenWidth,
    'getScreenHeight': getScreenHeight,
    'getScreenScale': getScreenScale,
    'setScreenSize': setScreenSize,
    '_internal': {
      'RENDEREDDOM': RENDEREDDOM,
      'SCREENWIDTH': SCREENWIDTH,
      'SCREENHEIGHT': SCREENHEIGHT,
      'SCREENSCALE': SCREENSCALE,
    }
  };
})();

var protoClClientSideRendering_client_input = (function() {
  /* --- import symbols --- */
  var sendJsonToServer = protoClClientSideRendering_client_socket.sendJsonToServer;
  var getScreenScale = protoClClientSideRendering_client_global.getScreenScale;
  var nameToCode = protoClClientSideRendering_protocol.nameToCode;
  var getRenderedDom = protoClClientSideRendering_client_global.getRenderedDom;
  /* --- define objects --- */
  function initInput() {
      window.addEventListener('keydown', onKeydown);
      window.addEventListener('keyup', onKeyup);
      window.addEventListener('mouseup', onMouseup);
      window.addEventListener('mousedown', onMousedown);
      __PS_MV_REG = [];
      return window.addEventListener('mousemove', onMousemove);
  };
  if ('undefined' === typeof KEYDOWNTABLE) {
      var KEYDOWNTABLE = {  };
  };
  function adjustKeyName(keyName) {
      if (keyName.startsWith('Arrow')) {
          __PS_MV_REG = [];
          return keyName.substr('Arrow'.length);
      } else if (keyName === ' ') {
          __PS_MV_REG = [];
          return 'space';
      } else {
          __PS_MV_REG = [];
          return keyName;
      };
  };
  function onKeydown(e) {
      var key138 = e.key;
      if (!KEYDOWNTABLE[key138]) {
          sendJsonToServer({ 'kind' : nameToCode('key-down'), 'data' : { 'key' : adjustKeyName(key138) } });
          __PS_MV_REG = [];
          return KEYDOWNTABLE[key138] = true;
      };
  };
  function onKeyup(e) {
      var key139 = e.key;
      sendJsonToServer({ 'kind' : nameToCode('key-up'), 'data' : { 'key' : adjustKeyName(key139) } });
      __PS_MV_REG = [];
      return KEYDOWNTABLE[key139] = null;
  };
  function calcAdjustedInputPoint(x, y) {
      var renderer = getRenderedDom();
      var canvas = renderer.querySelector('canvas');
      var scale = getScreenScale();
      var val140 = Math.floor((x - renderer.offsetLeft) / scale);
      __PS_MV_REG = [Math.floor(((canvas.height - y) + renderer.offsetTop) / scale)];
      return val140;
  };
  function mouseButtonToString(button) {
      switch (button) {
      case 0:
          return 'left';
      case 1:
          return 'center';
      case 2:
          return 'rihgt';
      default:
          return button;
      };
  };
  function sendMouseMessage(kind, e) {
      __PS_MV_REG = [];
      var x = calcAdjustedInputPoint(e.clientX, e.clientY);
      var y = __PS_MV_REG[0];
      __PS_MV_REG = [];
      return sendJsonToServer({ 'kind' : nameToCode(kind), 'data' : { 'button' : mouseButtonToString(e.button),
                                                                      'x' : x,
                                                                      'y' : y
                                                                    } });
  };
  function onMousedown(e) {
      __PS_MV_REG = [];
      return sendMouseMessage('mouse-down', e);
  };
  function onMouseup(e) {
      __PS_MV_REG = [];
      return sendMouseMessage('mouse-up', e);
  };
  function onMousemove(e) {
      __PS_MV_REG = [];
      return sendMouseMessage('mouse-move', e);
  };
  /* --- extern symbols --- */
  return {
    'initInput': initInput,
    '_internal': {
      'KEYDOWNTABLE': KEYDOWNTABLE,
      'adjustKeyName': adjustKeyName,
      'onKeydown': onKeydown,
      'onKeyup': onKeyup,
      'calcAdjustedInputPoint': calcAdjustedInputPoint,
      'mouseButtonToString': mouseButtonToString,
      'sendMouseMessage': sendMouseMessage,
      'onMousedown': onMousedown,
      'onMouseup': onMouseup,
      'onMousemove': onMousemove,
    }
  };
})();

var protoClClientSideRendering_client_core = (function() {
  /* --- import symbols --- */
  var setRenderedDom = protoClClientSideRendering_client_global.setRenderedDom;
  var setScreenSize = protoClClientSideRendering_client_global.setScreenSize;
  var interpretDrawCommand = protoClClientSideRendering_client_message.interpretDrawCommand;
  var dequeueDrawCommands = protoClClientSideRendering_client_message.dequeueDrawCommands;
  var initInput = protoClClientSideRendering_client_input.initInput;
  var registerSocketOnMessage = protoClClientSideRendering_client_socket.registerSocketOnMessage;
  var processMessage = protoClClientSideRendering_client_message.processMessage;
  /* --- define objects --- */
  function initCamera(offsetX, offsetY, width, height) {
      var x = offsetX;
      var y = offsetY;
      var z = 1000;
      var camera = new THREE.OrthographicCamera(x * -1, width - x, height - y, y * -1, 0, z * 2);
      camera.position.set(0, 0, z);
      __PS_MV_REG = [];
      return camera;
  };
  if ('undefined' === typeof WINDOWHEIGHTADJUST) {
      var WINDOWHEIGHTADJUST = 14;
  };
  function initializeScreenSize(renderedDom, renderer, screenWidth, screenHeight, resizeToScreenP) {
      RESIZETOSCREENP = resizeToScreenP;
      var calcScale = function () {
          __PS_MV_REG = [];
          return Math.min(window.innerWidth / screenWidth, (window.innerHeight - WINDOWHEIGHTADJUST) / screenHeight);
      };
      var setPositionBySize = function (width, height) {
          renderedDom.style.position = 'absolute';
          renderedDom.style.left = (window.innerWidth - width) / 2 + 'px';
          return renderedDom.style.top = (window.innerHeight - height) / 2 + 'px';
      };
      var setSize = function (width, height) {
          renderer.setSize(width, height);
          __PS_MV_REG = [];
          return setPositionBySize(width, height);
      };
      var resize = function () {
          var scale = RESIZETOSCREENP ? calcScale() : 1;
          setSize(screenWidth * scale, screenHeight * scale);
          __PS_MV_REG = [];
          return setScreenSize(screenWidth, screenHeight, scale);
      };
      resize();
      var resizeTimer = null;
      __PS_MV_REG = [];
      return window.addEventListener('resize', function (e) {
          if (resizeTimer) {
              clearTimeout(resizeTimer);
          };
          __PS_MV_REG = [];
          return resizeTimer = setTimeout(function () {
              __PS_MV_REG = [];
              return resize();
          }, 100);
      });
  };
  function start2dGame() {
      var _js142 = arguments.length;
      for (var n141 = 0; n141 < _js142; n141 += 2) {
          switch (arguments[n141]) {
          case 'screen-width':
              screenWidth = arguments[n141 + 1];
              break;
          case 'screen-height':
              screenHeight = arguments[n141 + 1];
              break;
          case 'rendered-dom':
              renderedDom = arguments[n141 + 1];
              break;
          case 'resize-to-screen-p':
              resizeToScreenP = arguments[n141 + 1];
              break;
          case 'init-function':
              initFunction = arguments[n141 + 1];
              break;
          case 'update-function':
              updateFunction = arguments[n141 + 1];
          };
      };
      var screenWidth;
      var screenHeight;
      var renderedDom;
      var resizeToScreenP = 'undefined' === typeof resizeToScreenP ? true : resizeToScreenP;
      var initFunction = 'undefined' === typeof initFunction ? function (scene) {
          return null;
      } : initFunction;
      var updateFunction = 'undefined' === typeof updateFunction ? function (scene) {
          return null;
      } : updateFunction;
      var scene = new THREE.Scene();
      var renderer = new THREE.WebGLRenderer;
      var camera = initCamera(0, 0, screenWidth, screenHeight);
      setRenderedDom(renderedDom);
      initializeScreenSize(renderedDom, renderer, screenWidth, screenHeight, resizeToScreenP);
      renderedDom.appendChild(renderer.domElement);
      var light = new THREE.DirectionalLight(0xffffff);
      light.position.set(0, 0.7, 0.7);
      scene.add(light);
      initFunction(scene);
      var renderLoop = function () {
          requestAnimationFrame(renderLoop);
          renderer.render(scene, camera);
          __PS_MV_REG = [];
          return updateFunction(scene);
      };
      __PS_MV_REG = [];
      return renderLoop();
  };
  function clearScene(scene) {
      while (scene.children.length > 0) {
          scene.remove(scene.children[0]);
      };
  };
  function updateDraw(scene) {
      var drawCommands = dequeueDrawCommands();
      if (drawCommands) {
          for (var command = null, _js_idx145 = 0; _js_idx145 < drawCommands.length; _js_idx145 += 1) {
              command = drawCommands[_js_idx145];
              interpretDrawCommand(scene, command);
          };
      };
  };
  start2dGame('screen-width', 800, 'screen-height', 600, 'rendered-dom', document.querySelector('#renderer'), 'update-function', updateDraw);
  function __psMainFunc__() {
      registerSocketOnMessage(processMessage);
      __PS_MV_REG = [];
      return initInput();
  };
  /* --- extern symbols --- */
  return {
    '_internal': {
      'initCamera': initCamera,
      'WINDOWHEIGHTADJUST': WINDOWHEIGHTADJUST,
      'initializeScreenSize': initializeScreenSize,
      'start2dGame': start2dGame,
      'clearScene': clearScene,
      'updateDraw': updateDraw,
      '__psMainFunc__': __psMainFunc__,
    }
  };
})();

protoClClientSideRendering_client_core._internal.__psMainFunc__();
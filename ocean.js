(function() {
  const canvas = document.getElementById('ocean-canvas');
  if (!canvas) return;

  const revealCard = () => {
    const cardContainer = document.querySelector('.card-container');
    if (cardContainer) cardContainer.classList.add('visible');
  };

  requestAnimationFrame(() => {
    document.body.classList.add('fade-bg');
  });

  const gl = canvas.getContext('webgl', { alpha: false, antialias: true }) ||
    canvas.getContext('experimental-webgl', { alpha: false, antialias: true });

  if (!gl) {
    canvas.classList.add('visible');
    setTimeout(revealCard, 100);
    return;
  }

  const vertexSource = `
    precision highp float;

    attribute vec3 a_position;
    attribute vec2 a_uv;

    uniform mat4 uProjection;
    uniform mat4 uView;
    uniform float uTime;

    varying vec2 vUv;
    varying vec3 vWorldPos;
    varying float vDepth;
    varying float vHeight;

    #define WAVE_SCALE 10.0

    float calculateSurface(vec2 p) {
      float y = 0.0;
      y += (sin(p.x * 0.95 / WAVE_SCALE + uTime * 0.52) + sin(p.x * 2.10 / WAVE_SCALE + uTime * 0.73) + sin(p.x * 3.15 / WAVE_SCALE + uTime * 0.23)) / 3.0;
      y += (sin(p.y * 0.35 / WAVE_SCALE + uTime * 0.65) + sin(p.y * 1.55 / WAVE_SCALE + uTime * 0.86) + sin(p.y * 2.75 / WAVE_SCALE + uTime * 0.37)) / 3.0;
      y += sin((p.x + p.y) * 0.42 / WAVE_SCALE + uTime * 0.41) * 0.28;
      return y;
    }

    void main() {
      vec3 pos = a_position;
      float wave = calculateSurface(pos.xz);
      float centerWave = calculateSurface(vec2(0.0, 0.0));

      // Keep the visible plane physically flat-ish in the distance, but still alive.
      float distanceFade = 1.0 - smoothstep(260.0, 860.0, length(pos.xz));
      pos.y += (wave - centerWave) * 1.55 * distanceFade;

      vec4 viewPos = uView * vec4(pos, 1.0);
      vUv = a_uv;
      vWorldPos = pos;
      vDepth = -viewPos.z;
      vHeight = pos.y;

      gl_Position = uProjection * viewPos;
    }
  `;

  const fragmentSource = `
    precision highp float;

    varying vec2 vUv;
    varying vec3 vWorldPos;
    varying float vDepth;
    varying float vHeight;

    uniform sampler2D uMap;
    uniform float uTextureReady;
    uniform float uTime;
    uniform vec3 uCameraPos;

    // lucumr.pocoo.org metaball palette.
    const vec3 WATER_DARK = vec3(0.040, 0.220, 0.440);   // #0a3870
    const vec3 WATER_MID = vec3(0.067, 0.361, 0.631);    // #115ca1
    const vec3 WATER_BRIGHT = vec3(0.100, 0.420, 0.700); // #1a6bb3
    const vec3 WATER_GLOW = vec3(0.320, 0.550, 0.820);   // dark-theme metaball bright
    const vec3 FOAM = vec3(0.900, 0.965, 1.000);

    #define WAVE_SCALE 10.0

    float hash(vec2 p) {
      return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123);
    }

    float noise(vec2 p) {
      vec2 i = floor(p);
      vec2 f = fract(p);
      f = f * f * (3.0 - 2.0 * f);
      return mix(
        mix(hash(i), hash(i + vec2(1.0, 0.0)), f.x),
        mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), f.x),
        f.y
      );
    }

    float calculateSurface(vec2 p) {
      float y = 0.0;
      y += (sin(p.x * 0.95 / WAVE_SCALE + uTime * 0.52) + sin(p.x * 2.10 / WAVE_SCALE + uTime * 0.73) + sin(p.x * 3.15 / WAVE_SCALE + uTime * 0.23)) / 3.0;
      y += (sin(p.y * 0.35 / WAVE_SCALE + uTime * 0.65) + sin(p.y * 1.55 / WAVE_SCALE + uTime * 0.86) + sin(p.y * 2.75 / WAVE_SCALE + uTime * 0.37)) / 3.0;
      y += sin((p.x + p.y) * 0.42 / WAVE_SCALE + uTime * 0.41) * 0.28;
      y += (noise(p * 0.032 + vec2(uTime * 0.018, -uTime * 0.014)) - 0.5) * 0.24;
      return y;
    }

    float waveHeightAt(vec2 p) {
      float distanceFade = 1.0 - smoothstep(260.0, 860.0, length(p));
      return (calculateSurface(p) - calculateSurface(vec2(0.0, 0.0))) * 1.55 * distanceFade;
    }

    vec3 waveNormal(vec2 p) {
      float e = 2.4;
      float hL = waveHeightAt(p - vec2(e, 0.0));
      float hR = waveHeightAt(p + vec2(e, 0.0));
      float hD = waveHeightAt(p - vec2(0.0, e));
      float hU = waveHeightAt(p + vec2(0.0, e));
      return normalize(vec3(hL - hR, e * 2.0, hD - hU));
    }

    float textureFoam(vec2 uv) {
      vec4 tex = texture2D(uMap, uv);
      return tex.a * uTextureReady;
    }

    float causticLines(vec2 p) {
      float a = sin(p.x * 0.18 + sin(p.y * 0.075 + uTime * 0.65) * 2.0 + uTime * 0.92);
      float b = sin(p.y * 0.15 + sin(p.x * 0.090 - uTime * 0.38) * 1.7 - uTime * 0.58);
      float c = sin((p.x - p.y) * 0.105 + uTime * 0.44);
      float v = (a + b + c) / 3.0;
      return smoothstep(0.58, 0.98, v);
    }

    void main() {
      vec2 world = vWorldPos.xz;
      vec3 normal = waveNormal(world);
      vec3 lightDir = normalize(vec3(0.44, 0.82, 0.36));
      vec3 viewDir = normalize(uCameraPos - vWorldPos);

      float nDotL = clamp(dot(normal, lightDir), 0.0, 1.0);
      float facing = clamp(dot(normal, viewDir), 0.0, 1.0);
      float fresnel = pow(1.0 - facing, 3.0);
      float crest = smoothstep(0.34, 1.05, vHeight);

      vec2 uv1 = vUv + vec2(uTime * -0.010, uTime * 0.013) + normal.xz * 0.020;
      vec2 uv2 = vUv * 1.75 + vec2(0.23, -0.17) + vec2(uTime * 0.007, uTime * -0.011) - normal.xz * 0.016;
      float foam1 = textureFoam(uv1);
      float foam2 = textureFoam(uv2);
      float linework = causticLines(world + normal.xz * 8.0);

      float shallowLight = smoothstep(115.0, 18.0, vDepth);
      float distanceWash = smoothstep(75.0, 260.0, vDepth);

      vec3 color = mix(WATER_DARK, WATER_MID, 0.62 + 0.22 * nDotL);
      color = mix(color, WATER_BRIGHT, 0.16 + distanceWash * 0.32 + fresnel * 0.18);
      color += WATER_GLOW * linework * (0.07 + shallowLight * 0.12);
      color += WATER_GLOW * crest * 0.08;
      color += FOAM * foam1 * (0.34 + crest * 0.30);
      color += FOAM * foam2 * 0.12;

      // Distance fades into water blue, not sky. The camera is always pitched down
      // enough that the full frustum intersects this mesh.
      color = mix(color, WATER_BRIGHT, distanceWash * 0.18);
      color *= 0.88 + 0.22 * nDotL;

      gl_FragColor = vec4(color, 1.0);
    }
  `;

  function createShader(type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      console.error(gl.getShaderInfoLog(shader));
      gl.deleteShader(shader);
      return null;
    }
    return shader;
  }

  function createProgram(vsSource, fsSource) {
    const vertexShader = createShader(gl.VERTEX_SHADER, vsSource);
    const fragmentShader = createShader(gl.FRAGMENT_SHADER, fsSource);
    if (!vertexShader || !fragmentShader) return null;

    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);

    gl.deleteShader(vertexShader);
    gl.deleteShader(fragmentShader);

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      console.error(gl.getProgramInfoLog(program));
      gl.deleteProgram(program);
      return null;
    }
    return program;
  }

  const program = createProgram(vertexSource, fragmentSource);
  if (!program) {
    canvas.classList.add('visible');
    setTimeout(revealCard, 100);
    return;
  }

  // A real water mesh in world space. The camera frustum is pitched downward so
  // every ray lands on this plane; no fullscreen fake projection and no sky pass.
  const segmentsX = 160;
  const segmentsZ = 160;
  const sizeX = 1300;
  const zNear = 180;
  const zFar = -950;
  const vertices = [];
  const uvs = [];
  const indices = [];

  for (let z = 0; z <= segmentsZ; z++) {
    const zT = z / segmentsZ;
    const pz = zNear + (zFar - zNear) * zT;
    for (let x = 0; x <= segmentsX; x++) {
      const xT = x / segmentsX;
      const px = (xT - 0.5) * sizeX;
      vertices.push(px, 0, pz);
      uvs.push(px / 58, pz / 58);
    }
  }

  for (let z = 0; z < segmentsZ; z++) {
    for (let x = 0; x < segmentsX; x++) {
      const i = z * (segmentsX + 1) + x;
      indices.push(i, i + 1, i + segmentsX + 1);
      indices.push(i + 1, i + segmentsX + 2, i + segmentsX + 1);
    }
  }

  const vertexBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);

  const uvBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(uvs), gl.STATIC_DRAW);

  const indexBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);

  const positionLoc = gl.getAttribLocation(program, 'a_position');
  const uvLoc = gl.getAttribLocation(program, 'a_uv');
  const projectionLoc = gl.getUniformLocation(program, 'uProjection');
  const viewLoc = gl.getUniformLocation(program, 'uView');
  const timeLoc = gl.getUniformLocation(program, 'uTime');
  const mapLoc = gl.getUniformLocation(program, 'uMap');
  const textureReadyLoc = gl.getUniformLocation(program, 'uTextureReady');
  const cameraPosLoc = gl.getUniformLocation(program, 'uCameraPos');

  const texture = gl.createTexture();
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(
    gl.TEXTURE_2D,
    0,
    gl.RGBA,
    1,
    1,
    0,
    gl.RGBA,
    gl.UNSIGNED_BYTE,
    new Uint8Array([255, 255, 255, 0])
  );
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);

  let textureReady = 0;
  const image = new Image();
  image.onload = () => {
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
    gl.generateMipmap(gl.TEXTURE_2D);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);

    const anisotropy = gl.getExtension('EXT_texture_filter_anisotropic') ||
      gl.getExtension('MOZ_EXT_texture_filter_anisotropic') ||
      gl.getExtension('WEBKIT_EXT_texture_filter_anisotropic');
    if (anisotropy) {
      const maxAnisotropy = gl.getParameter(anisotropy.MAX_TEXTURE_MAX_ANISOTROPY_EXT);
      gl.texParameterf(gl.TEXTURE_2D, anisotropy.TEXTURE_MAX_ANISOTROPY_EXT, Math.min(8, maxAnisotropy));
    }

    textureReady = 1;
  };
  image.src = './water.png';

  const cameraEye = [0, 38, 82];
  const cameraTarget = [0, -75, -130];
  const cameraUp = [0, 1, 0];
  const viewMatrix = lookAt(cameraEye, cameraTarget, cameraUp);

  let aspect = 1;
  function resize() {
    const dpr = Math.min(window.devicePixelRatio || 1, 2);
    const width = Math.max(1, Math.floor(window.innerWidth * dpr));
    const height = Math.max(1, Math.floor(window.innerHeight * dpr));
    if (canvas.width !== width || canvas.height !== height) {
      canvas.width = width;
      canvas.height = height;
    }
    aspect = canvas.width / canvas.height;
    gl.viewport(0, 0, canvas.width, canvas.height);
  }

  window.addEventListener('resize', resize);
  resize();

  function perspective(fov, aspect, near, far) {
    const f = 1.0 / Math.tan(fov / 2);
    const nf = 1 / (near - far);
    return new Float32Array([
      f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, (far + near) * nf, -1,
      0, 0, 2 * far * near * nf, 0,
    ]);
  }

  function lookAt(eye, target, up) {
    const zAxis = normalize([eye[0] - target[0], eye[1] - target[1], eye[2] - target[2]]);
    const xAxis = normalize(cross(up, zAxis));
    const yAxis = cross(zAxis, xAxis);
    return new Float32Array([
      xAxis[0], yAxis[0], zAxis[0], 0,
      xAxis[1], yAxis[1], zAxis[1], 0,
      xAxis[2], yAxis[2], zAxis[2], 0,
      -dot(xAxis, eye), -dot(yAxis, eye), -dot(zAxis, eye), 1,
    ]);
  }

  function normalize(v) {
    const len = Math.hypot(v[0], v[1], v[2]) || 1;
    return [v[0] / len, v[1] / len, v[2] / len];
  }

  function cross(a, b) {
    return [
      a[1] * b[2] - a[2] * b[1],
      a[2] * b[0] - a[0] * b[2],
      a[0] * b[1] - a[1] * b[0],
    ];
  }

  function dot(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  }

  gl.clearColor(0.10, 0.42, 0.70, 1.0);
  gl.enable(gl.DEPTH_TEST);
  gl.depthFunc(gl.LEQUAL);
  gl.disable(gl.CULL_FACE);

  const start = performance.now();
  function render(now) {
    resize();

    const t = (now - start) / 1000;
    const projectionMatrix = perspective(Math.PI * 42 / 180, aspect, 0.1, 1200);

    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.useProgram(program);

    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.enableVertexAttribArray(positionLoc);
    gl.vertexAttribPointer(positionLoc, 3, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffer);
    gl.enableVertexAttribArray(uvLoc);
    gl.vertexAttribPointer(uvLoc, 2, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(mapLoc, 0);
    gl.uniformMatrix4fv(projectionLoc, false, projectionMatrix);
    gl.uniformMatrix4fv(viewLoc, false, viewMatrix);
    gl.uniform1f(timeLoc, t);
    gl.uniform1f(textureReadyLoc, textureReady);
    gl.uniform3fv(cameraPosLoc, cameraEye);

    gl.drawElements(gl.TRIANGLES, indices.length, gl.UNSIGNED_SHORT, 0);
    requestAnimationFrame(render);
  }

  requestAnimationFrame(() => {
    canvas.classList.add('visible');
    setTimeout(revealCard, 100);
  });
  requestAnimationFrame(render);
})();

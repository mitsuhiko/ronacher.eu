(function() {
  const canvas = document.getElementById('ocean-canvas');
  const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
  
  // Start fade from black to horizon gray
  requestAnimationFrame(() => {
    document.body.classList.add('fade-bg');
  });
  
  if (!gl) return;

  let aspect = 1;
  function resize() {
    const dpr = window.devicePixelRatio || 1;
    canvas.width = window.innerWidth * dpr;
    canvas.height = window.innerHeight * dpr;
    aspect = canvas.width / canvas.height;
    gl.viewport(0, 0, canvas.width, canvas.height);
  }
  resize();
  window.addEventListener('resize', resize);

  // === BACKGROUND SHADER (gradient sky) ===
  const bgVertexSource = `
    attribute vec2 a_position;
    varying vec2 vUv;
    void main() {
      vUv = a_position * 0.5 + 0.5;
      gl_Position = vec4(a_position, 0.999, 1.0);
    }
  `;
  const bgFragmentSource = `
    precision highp float;
    varying vec2 vUv;
    void main() {
      vec3 topColor = vec3(0.08, 0.07, 0.065);
      vec3 bottomColor = vec3(0.14, 0.13, 0.12);
      vec3 color = mix(bottomColor, topColor, vUv.y);
      gl_FragColor = vec4(color, 1.0);
    }
  `;

  // === OCEAN SHADER ===
  const oceanVertexSource = `
    attribute vec3 a_position;
    attribute vec2 a_uv;
    uniform mat4 uProjection;
    uniform mat4 uView;
    uniform float uTime;
    varying vec2 vUv;
    varying float vHeight;
    varying vec3 vWorldPos;

    #define SCALE 10.0

    float calculateSurface(float x, float z) {
      float y = 0.0;
      y += (sin(x * 1.0 / SCALE + uTime * 0.5) + sin(x * 2.3 / SCALE + uTime * 0.75) + sin(x * 3.3 / SCALE + uTime * 0.2)) / 3.0;
      y += (sin(z * 0.2 / SCALE + uTime * 0.9) + sin(z * 1.8 / SCALE + uTime * 0.9) + sin(z * 2.8 / SCALE + uTime * 0.4)) / 3.0;
      return y;
    }

    void main() {
      vUv = a_uv;
      vec3 pos = a_position;
      
      // Reduce wave amplitude with distance for cleaner horizon
      float distFromCenter = length(pos.xz) / 400.0;
      float strength = 1.5 * (1.0 - smoothstep(0.0, 1.0, distFromCenter));
      
      pos.y += strength * calculateSurface(pos.x, pos.z);
      pos.y -= strength * calculateSurface(0.0, 0.0);
      vHeight = pos.y;
      vWorldPos = pos;

      gl_Position = uProjection * uView * vec4(pos, 1.0);
    }
  `;

    const hasDerivatives = !!gl.getExtension('OES_standard_derivatives');
    const oceanFragmentHeader = hasDerivatives
      ? '#extension GL_OES_standard_derivatives : enable\n#define HAS_DERIVATIVES 1\n'
      : '#define HAS_DERIVATIVES 0\n';

    const oceanFragmentSource = oceanFragmentHeader + `
      precision highp float;
      varying vec2 vUv;
      varying float vHeight;
      varying vec3 vWorldPos;
      uniform sampler2D uMap;
      uniform float uTime;
      uniform vec3 uLightDir;
      uniform vec3 uBarrelCenter;
      uniform float uBarrelRadius;
      uniform float uBarrelHalfHeight;
      uniform float uShadowDebug;

      #define MAX_RIPPLES 12
      uniform float uRippleTimes[MAX_RIPPLES];
      uniform float uRippleAmps[MAX_RIPPLES];
      uniform float uRippleSpeed;
      uniform float uRippleMaxAge;
      uniform float uRippleWidth;
      uniform float uRippleStrength;

      const vec3 uColor = vec3(0.165, 0.153, 0.145);
      const vec3 uLightColor = vec3(0.28, 0.25, 0.22);
      const vec3 uDarkColor = vec3(0.11, 0.10, 0.09);
      const vec3 uFogColor = vec3(0.11, 0.10, 0.09);

      #define SCALE 10.0

      float calculateSurface(float x, float z) {
        float y = 0.0;
        y += (sin(x * 1.0 / SCALE + uTime * 0.5) + sin(x * 2.3 / SCALE + uTime * 0.75) + sin(x * 3.3 / SCALE + uTime * 0.2)) / 3.0;
        y += (sin(z * 0.2 / SCALE + uTime * 0.9) + sin(z * 1.8 / SCALE + uTime * 0.9) + sin(z * 2.8 / SCALE + uTime * 0.4)) / 3.0;
        return y;
      }

      float waveHeightAt(vec2 xz) {
        float base = calculateSurface(0.0, 0.0);
        float distFromCenter = length(xz) / 400.0;
        float strength = 1.5 * (1.0 - smoothstep(0.0, 1.0, distFromCenter));
        return strength * (calculateSurface(xz.x, xz.y) - base);
      }

      float waveHeightAtWithBase(vec2 xz, float base) {
        float distFromCenter = length(xz) / 400.0;
        float strength = 1.5 * (1.0 - smoothstep(0.0, 1.0, distFromCenter));
        return strength * (calculateSurface(xz.x, xz.y) - base);
      }

      float barrelShadowAt(vec3 worldPos) {
        vec3 lightDir = normalize(uLightDir);
        float lenXZ = length(lightDir.xz);
        if (lightDir.y <= 0.001 || lenXZ <= 0.001) return 0.0;

        vec2 shadowDir = normalize(-lightDir.xz);
        vec2 perpDir = vec2(-shadowDir.y, shadowDir.x);

        vec2 delta = worldPos.xz - uBarrelCenter.xz;
        float s = dot(delta, shadowDir);
        float t = dot(delta, perpDir);

        float topY = uBarrelCenter.y + uBarrelHalfHeight;
        float maxS = (topY - worldPos.y) * (lenXZ / max(lightDir.y, 0.001));
        maxS *= 4.0;
        if (maxS <= 0.0) return 0.0;

        float forwardMask = step(0.0, s);
        float lengthMask = 1.0 - smoothstep(maxS * 0.75, maxS, s);
        float startFade = smoothstep(0.0, 0.9, s);

        float width = uBarrelRadius * 1.15 + s * 0.10;
        float core = 1.0 - smoothstep(width, width + 2.0, abs(t));

        return core * forwardMask * lengthMask * startFade;
      }

      float waveShadowAt(vec3 worldPos) {
        // Use consistent light direction for entire ocean (directional light)
        vec3 lightDir = normalize(uLightDir);
        vec2 lightXZ = normalize(lightDir.xz);
        
        // Sample wave height gradient toward light
        float eps = 2.0;
        float hTowardLight = waveHeightAt(worldPos.xz + lightXZ * eps);
        float hAwayFromLight = waveHeightAt(worldPos.xz - lightXZ * eps);
        
        // Slope toward light (positive = rising toward light)
        float slope = (hTowardLight - hAwayFromLight) / (2.0 * eps);
        
        // For a heightfield, faces that rise toward the light point away from it
        // (normal ~ (-∇h, 1)), so positive slope means "back-facing" and should darken.
        float shadow = smoothstep(-0.06, 0.04, slope);
        return shadow;
      }

      float rippleAt(vec2 worldXZ) {
        vec2 p = worldXZ - uBarrelCenter.xz;
        float d = length(p);
        float a = atan(p.y, p.x);

        float m = 0.0;
        for (int i = 0; i < MAX_RIPPLES; i++) {
          float age = uTime - uRippleTimes[i];
          float amp = uRippleAmps[i];
          float alive = step(0.0, age) * (1.0 - step(uRippleMaxAge, age));

          float r0 = uBarrelRadius * 1.1 - 0.15;
          float r = r0 + age * uRippleSpeed;
          r *= 1.0 + 0.02 * sin(a * 6.0 + age * 5.0 + float(i) * 1.37);

          float distToRing = abs(d - r);
          float aa = 0.035 + 0.0015 * d;
          #if HAS_DERIVATIVES
            aa = max(fwidth(d), 0.02);
          #endif
          float band = smoothstep(uRippleWidth + aa, uRippleWidth, distToRing);
          // Toon-ish threshold with just enough AA to avoid shimmer.
          band = smoothstep(0.75 - aa * 6.0, 0.75 + aa * 6.0, band);

          // Keep the ring fully present right when it spawns, then fade.
          float fadeStart = 0.22;
          float fadeLen = max(uRippleMaxAge - fadeStart, 0.001);
          float tNorm = max(0.0, age - fadeStart) / fadeLen;
          float ageFade = (1.0 - smoothstep(fadeStart, uRippleMaxAge, age)) * exp(-tNorm * 2.7);
          // Only fade out near the outer limit; don't dim near the origin.
          float maxR = r0 + uRippleSpeed * uRippleMaxAge;
          float distFade = 1.0 - smoothstep(maxR * 0.78, maxR, d);

          m += alive * band * ageFade * distFade * amp;
        }
        return clamp(m, 0.0, 1.0);
      }

      void main() {
        vec2 uv = vUv * 20.0 + vec2(uTime * -0.05);

      uv.y += 0.01 * (sin(uv.x * 3.5 + uTime * 0.35) + sin(uv.x * 4.8 + uTime * 1.05) + sin(uv.x * 7.3 + uTime * 0.45)) / 3.0;
      uv.x += 0.12 * (sin(uv.y * 4.0 + uTime * 0.5) + sin(uv.y * 6.8 + uTime * 0.75) + sin(uv.y * 11.3 + uTime * 0.2)) / 3.0;
      uv.y += 0.12 * (sin(uv.x * 4.2 + uTime * 0.64) + sin(uv.x * 6.3 + uTime * 1.65) + sin(uv.x * 8.2 + uTime * 0.45)) / 3.0;

      vec4 tex1 = texture2D(uMap, uv);
      // Slight, slow drift for the darker "lower water" layer.
      vec2 uv2 = uv + vec2(0.2);
      uv2 += 0.02 * vec2(sin(uTime * 0.07), cos(uTime * 0.05));
      vec4 tex2 = texture2D(uMap, uv2);

        vec3 lightDir = normalize(uLightDir);
        float base = calculateSurface(0.0, 0.0);
        float nEps = 2.0;
        float hx1 = waveHeightAtWithBase(vWorldPos.xz + vec2(nEps, 0.0), base);
        float hx0 = waveHeightAtWithBase(vWorldPos.xz - vec2(nEps, 0.0), base);
        float hz1 = waveHeightAtWithBase(vWorldPos.xz + vec2(0.0, nEps), base);
        float hz0 = waveHeightAtWithBase(vWorldPos.xz - vec2(0.0, nEps), base);
        float dHdX = (hx1 - hx0) / (2.0 * nEps);
        float dHdZ = (hz1 - hz0) / (2.0 * nEps);
        vec3 waveNormal = normalize(vec3(-dHdX, 1.0, -dHdZ));
        float NdotL = clamp(dot(waveNormal, lightDir), 0.0, 1.0);

        vec3 color = uColor;
        color += uLightColor * tex1.a * 0.7;
        color -= uDarkColor * tex2.a * 0.3;
        color += vec3(0.02) * vHeight;

        float barrelShadow = barrelShadowAt(vWorldPos);
        float waveShadow = waveShadowAt(vWorldPos);
        if (uShadowDebug > 0.5) {
          float m = clamp(barrelShadow + 0.7 * waveShadow, 0.0, 1.0);
          gl_FragColor = vec4(vec3(m), 1.0);
          return;
        }
        float shade = 1.0 - 0.45 * barrelShadow - 0.38 * waveShadow;
        color *= clamp(shade, 0.35, 1.0);

        float ripples = rippleAt(vWorldPos.xz);
        color += uLightColor * (ripples * uRippleStrength) * (0.35 + 0.65 * NdotL);

        // Directional diffuse lighting so highlights track the sun direction.
        color *= mix(0.78, 1.10, NdotL);

        // Radial fog - fade edges and distance
        float distZ = -vWorldPos.z;
        float distX = abs(vWorldPos.x);
        float fogZ = smoothstep(30.0, 350.0, distZ);
        float fogX = smoothstep(150.0, 350.0, distX);
        float fog = max(fogZ, fogX);
        color = mix(color, uFogColor, fog);

      gl_FragColor = vec4(color, 1.0);
    }
  `;

  function createShader(type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      console.error(gl.getShaderInfoLog(shader));
      return null;
    }
    return shader;
  }

  function createProgram(vs, fs) {
    const program = gl.createProgram();
    gl.attachShader(program, createShader(gl.VERTEX_SHADER, vs));
    gl.attachShader(program, createShader(gl.FRAGMENT_SHADER, fs));
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      console.error(gl.getProgramInfoLog(program));
    }
    return program;
  }

  const bgProgram = createProgram(bgVertexSource, bgFragmentSource);
  const oceanProgram = createProgram(oceanVertexSource, oceanFragmentSource);

  // === BARREL SHADER ===
  const barrelVertexSource = `
    attribute vec3 a_position;
    attribute vec3 a_normal;
    uniform mat4 uProjection;
    uniform mat4 uView;
    uniform float uTime;
    uniform vec3 uBarrelPos;
    varying vec3 vNormal;
    varying vec3 vWorldPos;
    varying float vY;

    #define SCALE 10.0

    float calculateSurface(float x, float z) {
      float y = 0.0;
      y += (sin(x * 1.0 / SCALE + uTime * 0.5) + sin(x * 2.3 / SCALE + uTime * 0.75) + sin(x * 3.3 / SCALE + uTime * 0.2)) / 3.0;
      y += (sin(z * 0.2 / SCALE + uTime * 0.9) + sin(z * 1.8 / SCALE + uTime * 0.9) + sin(z * 2.8 / SCALE + uTime * 0.4)) / 3.0;
      return y;
    }

    void main() {
      vY = a_position.y;
      
      // Calculate wave height at barrel position
      float waveHeight = 1.5 * calculateSurface(uBarrelPos.x, uBarrelPos.z);
      waveHeight -= 1.5 * calculateSurface(0.0, 0.0);
      
      // Calculate tilt from wave slope (sample nearby points)
      float sampleDist = 2.0;
      float hLeft = calculateSurface(uBarrelPos.x - sampleDist, uBarrelPos.z);
      float hRight = calculateSurface(uBarrelPos.x + sampleDist, uBarrelPos.z);
      float hFront = calculateSurface(uBarrelPos.x, uBarrelPos.z - sampleDist);
      float hBack = calculateSurface(uBarrelPos.x, uBarrelPos.z + sampleDist);
      
      float tiltX = (hRight - hLeft) * 0.4;
      float tiltZ = (hBack - hFront) * 0.4;
      
      // Base tilt with gentle wobble
      float baseTilt = 0.25 + 0.1 * sin(uTime * 0.7);
      
      // Apply rotation around barrel center (y=1.12)
      vec3 pos = a_position;
      pos.y -= 1.12;
      
      // Rotate around Z axis (left-right tilt)
      float cz = cos(tiltX + baseTilt);
      float sz = sin(tiltX + baseTilt);
      vec3 rotZ = vec3(pos.x * cz - pos.y * sz, pos.x * sz + pos.y * cz, pos.z);
      
      // Rotate around X axis (front-back tilt)
      float cx = cos(tiltZ);
      float sx = sin(tiltZ);
      vec3 rotXZ = vec3(rotZ.x, rotZ.y * cx - rotZ.z * sx, rotZ.y * sx + rotZ.z * cx);
      
      // Apply same rotation to normals
      vec3 norm = a_normal;
      vec3 normRotZ = vec3(norm.x * cz - norm.y * sz, norm.x * sz + norm.y * cz, norm.z);
      vec3 normRotXZ = vec3(normRotZ.x, normRotZ.y * cx - normRotZ.z * sx, normRotZ.y * sx + normRotZ.z * cx);
      vNormal = normRotXZ;
      
      pos = rotXZ;
      pos.y += 1.12;
      
      // Position barrel on wave, partially submerged
      pos += uBarrelPos;
      pos.y += waveHeight - 0.8;
      
      vWorldPos = pos;
      gl_Position = uProjection * uView * vec4(pos, 1.0);
    }
  `;

    const barrelFragmentSource = `
      precision highp float;
      varying vec3 vNormal;
      varying vec3 vWorldPos;
      varying float vY;
      uniform vec3 uLightDir;
      uniform vec3 uCameraPos;

      void main() {
        vec3 normal = normalize(vNormal);
        vec3 lightDir = normalize(uLightDir);
        float NdotL = dot(normal, lightDir);
      
      // Detect top/bottom caps by normal direction
      float isTopCap = step(0.8, normal.y);
      float isBottomCap = step(0.8, -normal.y);
      float isCap = max(isTopCap, isBottomCap);
      
      // Add fill light for caps so they're not in shadow
      float fillNdotL = NdotL + isCap * 0.5;
      
      // Wind Waker style cel shading - hard cutoffs for 3 tones
      float toon;
      if (fillNdotL > 0.4) {
        toon = 1.0;           // Lit
      } else if (fillNdotL > -0.2) {
        toon = 0.7;           // Mid-tone
      } else {
        toon = 0.45;          // Shadow
      }
      
      // Barrel colors - warm wood with darker metal bands (Wind Waker palette)
      vec3 woodLight = vec3(0.72, 0.52, 0.32);   // Warm light wood
      vec3 woodMid = vec3(0.55, 0.38, 0.22);     // Mid wood
      vec3 woodDark = vec3(0.38, 0.24, 0.14);    // Shadow wood
      
      vec3 metalLight = vec3(0.35, 0.28, 0.22);  // Light metal
      vec3 metalMid = vec3(0.25, 0.20, 0.16);    // Mid metal
      vec3 metalDark = vec3(0.15, 0.12, 0.10);   // Dark metal
      
      // Metal bands at top edge, bottom edge, and middle - only on sides, not on caps
      float isBand = 0.0;
      if (isCap < 0.5) {
        float h = vY / 2.24;  // Normalize to 0-1
        if (h > 0.88 || h < 0.12 || (h > 0.42 && h < 0.58)) {
          isBand = 1.0;
        }
      }
      
      // Select color based on toon level
      vec3 color;
      if (isBand > 0.5) {
        if (toon > 0.9) color = metalLight;
        else if (toon > 0.6) color = metalMid;
        else color = metalDark;
      } else {
        if (toon > 0.9) color = woodLight;
        else if (toon > 0.6) color = woodMid;
        else color = woodDark;
      }
        
        // Subtle rim highlight for that Wind Waker glow
        vec3 viewDir = normalize(uCameraPos - vWorldPos);
        float rim = 1.0 - max(dot(viewDir, normal), 0.0);
        rim = smoothstep(0.6, 1.0, rim);
        color += vec3(0.15, 0.12, 0.08) * rim;
      
      gl_FragColor = vec4(color, 1.0);
    }
  `;

  const barrelProgram = createProgram(barrelVertexSource, barrelFragmentSource);

  // Background quad
  const bgBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, bgBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1,-1, 1,-1, -1,1, -1,1, 1,-1, 1,1]), gl.STATIC_DRAW);

  // Ocean plane geometry - large plane extending to horizon
  const segments = 128;
  const sizeX = 800;
  const sizeZ = 600;
  const oceanVerts = [];
  const oceanUvs = [];
  const oceanIndices = [];
  
  for (let z = 0; z <= segments; z++) {
    for (let x = 0; x <= segments; x++) {
      const px = (x / segments - 0.5) * sizeX;
      // Offset Z so plane extends from camera into distance
      const pz = (z / segments) * -sizeZ + 40;
      oceanVerts.push(px, 0, pz);
      oceanUvs.push(x / segments * 8, z / segments * 6);
    }
  }
  for (let z = 0; z < segments; z++) {
    for (let x = 0; x < segments; x++) {
      const i = z * (segments + 1) + x;
      oceanIndices.push(i, i + 1, i + segments + 1);
      oceanIndices.push(i + 1, i + segments + 2, i + segments + 1);
    }
  }

  const oceanVertBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, oceanVertBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(oceanVerts), gl.STATIC_DRAW);

  const oceanUvBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, oceanUvBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(oceanUvs), gl.STATIC_DRAW);

  const oceanIndexBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, oceanIndexBuffer);
  const ext = gl.getExtension('OES_element_index_uint');
  const useUint32 = ext && oceanIndices.length > 65535;
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, useUint32 ? new Uint32Array(oceanIndices) : new Uint16Array(oceanIndices), gl.STATIC_DRAW);
  const indexType = useUint32 ? gl.UNSIGNED_INT : gl.UNSIGNED_SHORT;

  // Barrel geometry - cylinder with bulge and proper normals
  const barrelVerts = [];
  const barrelNormals = [];
  const barrelIndices = [];
  const barrelSegments = 24;
  const barrelRings = 12;
  const barrelHeight = 2.24;
  const barrelRadius = 0.8;
  const bulgeFactor = 0.18;
  
  // Bulge function: returns radius multiplier at normalized height t (0-1)
  function bulge(t) {
    return 1.0 + bulgeFactor * Math.sin(t * Math.PI);
  }
  
  // Derivative of bulge for normal calculation
  function bulgeDeriv(t) {
    return bulgeFactor * Math.PI * Math.cos(t * Math.PI);
  }
  
  // Create side vertices with proper normals accounting for bulge
  const sideStartIdx = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    const cosT = Math.cos(theta);
    const sinT = Math.sin(theta);
    
    for (let j = 0; j <= barrelRings; j++) {
      const t = j / barrelRings;
      const y = t * barrelHeight;
      const r = barrelRadius * bulge(t);
      
      barrelVerts.push(cosT * r, y, sinT * r);
      
      // Normal: account for bulge slope
      // The surface curves outward, so normal tilts up/down based on bulge derivative
      const dRadius = barrelRadius * bulgeDeriv(t) / barrelHeight;
      const nx = cosT;
      const ny = -dRadius;
      const nz = sinT;
      const len = Math.sqrt(nx*nx + ny*ny + nz*nz);
      barrelNormals.push(nx/len, ny/len, nz/len);
    }
  }
  
  // Side indices
  const ringVerts = barrelRings + 1;
  for (let i = 0; i < barrelSegments; i++) {
    for (let j = 0; j < barrelRings; j++) {
      const a = sideStartIdx + i * ringVerts + j;
      const b = sideStartIdx + (i + 1) * ringVerts + j;
      barrelIndices.push(a, b, a + 1);
      barrelIndices.push(b, b + 1, a + 1);
    }
  }
  
  // Top cap with inset - like a real barrel with rim and recessed lid
  const topRadius = barrelRadius * bulge(1.0);
  const rimWidth = 0.12;
  const insetDepth = 0.15;
  const innerRadius = topRadius - rimWidth;
  const rimHeight = barrelHeight;
  const insetHeight = barrelHeight - insetDepth;
  
  // Outer rim ring (at full height)
  const rimOuterStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * topRadius, rimHeight, Math.sin(theta) * topRadius);
    barrelNormals.push(0, 1, 0);
  }
  
  // Inner rim ring (at full height, forms the rim top surface)
  const rimInnerStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * innerRadius, rimHeight, Math.sin(theta) * innerRadius);
    barrelNormals.push(0, 1, 0);
  }
  
  // Rim top surface (ring between outer and inner)
  for (let i = 0; i < barrelSegments; i++) {
    barrelIndices.push(rimOuterStart + i, rimOuterStart + i + 1, rimInnerStart + i);
    barrelIndices.push(rimOuterStart + i + 1, rimInnerStart + i + 1, rimInnerStart + i);
  }
  
  // Inner wall of rim (vertical, pointing inward)
  const rimWallTopStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * innerRadius, rimHeight, Math.sin(theta) * innerRadius);
    barrelNormals.push(-Math.cos(theta), 0, -Math.sin(theta));
  }
  
  const rimWallBottomStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * innerRadius, insetHeight, Math.sin(theta) * innerRadius);
    barrelNormals.push(-Math.cos(theta), 0, -Math.sin(theta));
  }
  
  // Inner wall faces
  for (let i = 0; i < barrelSegments; i++) {
    barrelIndices.push(rimWallTopStart + i, rimWallBottomStart + i, rimWallTopStart + i + 1);
    barrelIndices.push(rimWallBottomStart + i, rimWallBottomStart + i + 1, rimWallTopStart + i + 1);
  }
  
  // Inset lid (recessed center)
  const topCenterIdx = barrelVerts.length / 3;
  barrelVerts.push(0, insetHeight, 0);
  barrelNormals.push(0, 1, 0);
  
  const topEdgeStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * innerRadius, insetHeight, Math.sin(theta) * innerRadius);
    barrelNormals.push(0, 1, 0);
  }
  
  for (let i = 0; i < barrelSegments; i++) {
    barrelIndices.push(topCenterIdx, topEdgeStart + i, topEdgeStart + i + 1);
  }
  
  // Bottom cap - separate vertices with down-facing normals
  const bottomCenterIdx = barrelVerts.length / 3;
  const bottomRadius = barrelRadius * bulge(0.0);
  barrelVerts.push(0, 0, 0);
  barrelNormals.push(0, -1, 0);
  
  const bottomEdgeStart = barrelVerts.length / 3;
  for (let i = 0; i <= barrelSegments; i++) {
    const theta = (i / barrelSegments) * Math.PI * 2;
    barrelVerts.push(Math.cos(theta) * bottomRadius, 0, Math.sin(theta) * bottomRadius);
    barrelNormals.push(0, -1, 0);
  }
  
  for (let i = 0; i < barrelSegments; i++) {
    barrelIndices.push(bottomCenterIdx, bottomEdgeStart + i + 1, bottomEdgeStart + i);
  }

  const barrelVertBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, barrelVertBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(barrelVerts), gl.STATIC_DRAW);

  const barrelNormalBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, barrelNormalBuffer);
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(barrelNormals), gl.STATIC_DRAW);

  const barrelIndexBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, barrelIndexBuffer);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(barrelIndices), gl.STATIC_DRAW);
  
    // Barrel position will be calculated from screen coordinates
    const barrelPos = [0, 0, 0];
    const barrelCenter = [0, 0, 0];
    const cameraEye = [0, 8, 45];
    const cameraTarget = [0, -2, -100];
    const lightDir = [0, 0, 0];
    const lightDirView = normalize([0.7, 0.35, 0.62]); // front-right, slightly above (view space)

    // Expanding ring pulses around the barrel (Wind Waker-ish ripples)
    const MAX_RIPPLES = 8;
    const rippleTimes = new Float32Array(MAX_RIPPLES);
    const rippleAmps = new Float32Array(MAX_RIPPLES);
    rippleTimes.fill(-1e6);
    rippleAmps.fill(0);
    let rippleCursor = 0;
    let lastRippleSpawn = -1e9;
    let lastRenderT = null;
    let prevBarrelWaveHeight = 0;

    const RIPPLE_BASE_SPEED = 0.6;
    const RIPPLE_BASE_MAX_AGE = 0.8;
    const RIPPLE_BASE_INTERVAL = 0.4;

    // Target: half the speed, three times the travel distance.
    const RIPPLE_SPEED_SCALE = 0.5;
    const RIPPLE_DISTANCE_SCALE = 3.0;

    const rippleSpeed = RIPPLE_BASE_SPEED * RIPPLE_SPEED_SCALE;
    const rippleTravelDistance = (RIPPLE_BASE_SPEED * RIPPLE_BASE_MAX_AGE) * RIPPLE_DISTANCE_SCALE;
    const rippleMaxAge = rippleTravelDistance / Math.max(0.001, rippleSpeed);

    // Keep ripple spacing stable in world units (not time), and avoid overwriting
    // ripples before they reach the outer radius.
    const rippleSpacing = RIPPLE_BASE_SPEED * RIPPLE_BASE_INTERVAL;
    const rippleIntervalFromSpacing = rippleSpacing / Math.max(0.001, rippleSpeed);
    const minIntervalToAvoidOverwrite = rippleMaxAge / Math.max(1, (MAX_RIPPLES - 1));
    const rippleInterval = Math.max(rippleIntervalFromSpacing, minIntervalToAvoidOverwrite);

    function spawnRipple(t, amp) {
      rippleTimes[rippleCursor] = t;
      rippleAmps[rippleCursor] = amp;
      rippleCursor = (rippleCursor + 1) % MAX_RIPPLES;
    }
    
    // Unproject screen coords to world position on ocean plane (y=0)
    function screenToWorld(screenX, screenY, projMatrix, viewMat) {
    // Convert screen coords to NDC (-1 to 1)
    const ndcX = screenX * 2 - 1;
    const ndcY = 1 - screenY * 2;
    
    // Invert projection matrix
    const f = 1.0 / Math.tan(Math.PI / 8); // fov/2
    const nf = 1 / (0.1 - 800);
    const invProj = [
      aspect / f, 0, 0, 0,
      0, 1 / f, 0, 0,
      0, 0, 0, 1 / (2 * 800 * 0.1 * nf),
      0, 0, -1, (800 + 0.1) / (2 * 800 * 0.1)
    ];
    
      // Camera position and direction from view matrix
      const forward = normalize([cameraTarget[0] - cameraEye[0], cameraTarget[1] - cameraEye[1], cameraTarget[2] - cameraEye[2]]);
      const right = normalize(cross([0, 1, 0], [-forward[0], -forward[1], -forward[2]]));
      const up = cross([-forward[0], -forward[1], -forward[2]], right);
    
    // Calculate ray direction in world space
    const fovTan = Math.tan(Math.PI / 8);
    const rayX = right[0] * ndcX * fovTan * aspect + up[0] * ndcY * fovTan + forward[0];
    const rayY = right[1] * ndcX * fovTan * aspect + up[1] * ndcY * fovTan + forward[1];
    const rayZ = right[2] * ndcX * fovTan * aspect + up[2] * ndcY * fovTan + forward[2];
    const rayDir = normalize([rayX, rayY, rayZ]);
    
      // Intersect ray with y=0 plane
      if (Math.abs(rayDir[1]) < 0.001) return [cameraEye[0], 0, cameraEye[2]];
      const t = -cameraEye[1] / rayDir[1];
      return [cameraEye[0] + rayDir[0] * t, 0, cameraEye[2] + rayDir[2] * t];
    }

  // Load texture with mipmapping
  const texture = gl.createTexture();
  const image = new Image();
  image.crossOrigin = 'anonymous';
  image.onload = function() {
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
    gl.generateMipmap(gl.TEXTURE_2D);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    
    // Fade in the WebGL canvas
    canvas.classList.add('visible');
    
    // Fade in the card 100ms after canvas starts fading
    setTimeout(() => {
      const cardContainer = document.querySelector('.card-container');
      if (cardContainer) cardContainer.classList.add('visible');
    }, 100);
  };
  image.src = './water.png';

  // Matrix helpers
  function perspective(fov, aspect, near, far) {
    const f = 1.0 / Math.tan(fov / 2);
    const nf = 1 / (near - far);
    return new Float32Array([
      f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, (far + near) * nf, -1,
      0, 0, 2 * far * near * nf, 0
    ]);
  }

  function lookAt(eye, target, up) {
    const zAxis = normalize([eye[0]-target[0], eye[1]-target[1], eye[2]-target[2]]);
    const xAxis = normalize(cross(up, zAxis));
    const yAxis = cross(zAxis, xAxis);
    return new Float32Array([
      xAxis[0], yAxis[0], zAxis[0], 0,
      xAxis[1], yAxis[1], zAxis[1], 0,
      xAxis[2], yAxis[2], zAxis[2], 0,
      -dot(xAxis, eye), -dot(yAxis, eye), -dot(zAxis, eye), 1
    ]);
  }

  function normalize(v) {
    const len = Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    return [v[0]/len, v[1]/len, v[2]/len];
  }
  function cross(a, b) {
    return [a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]];
  }
  function dot(a, b) {
    return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
  }

    function calculateSurfaceJS(x, z, t) {
      const SCALE = 10.0;
      let y = 0;
      y += (Math.sin(x * 1.0 / SCALE + t * 0.5) + Math.sin(x * 2.3 / SCALE + t * 0.75) + Math.sin(x * 3.3 / SCALE + t * 0.2)) / 3.0;
      y += (Math.sin(z * 0.2 / SCALE + t * 0.9) + Math.sin(z * 1.8 / SCALE + t * 0.9) + Math.sin(z * 2.8 / SCALE + t * 0.4)) / 3.0;
      return y;
    }

    function barrelWaveHeightAt(x, z, t) {
      return 1.5 * calculateSurfaceJS(x, z, t) - 1.5 * calculateSurfaceJS(0.0, 0.0, t);
    }

    const viewMatrix = lookAt(cameraEye, cameraTarget, [0, 1, 0]);
    function updateLightDir() {
      // Build camera basis vectors in world space.
      const forward = normalize([
        cameraTarget[0] - cameraEye[0],
        cameraTarget[1] - cameraEye[1],
        cameraTarget[2] - cameraEye[2],
      ]);
      const right = normalize(cross(forward, [0, 1, 0]));
      const up = cross(right, forward);

      // View +Z points toward the camera; convert view-space to world-space.
      const vx = lightDirView[0];
      const vy = lightDirView[1];
      const vz = lightDirView[2];
      const world = normalize([
        right[0] * vx + up[0] * vy + (-forward[0]) * vz,
        right[1] * vx + up[1] * vy + (-forward[1]) * vz,
        right[2] * vx + up[2] * vy + (-forward[2]) * vz,
      ]);

      lightDir[0] = world[0];
      lightDir[1] = world[1];
      lightDir[2] = world[2];
    }
    updateLightDir();

    function render(time) {
    const t = time * 0.001;
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.enable(gl.DEPTH_TEST);

      // Update projection for aspect - far plane at 800 for horizon
      const proj = perspective(Math.PI / 4, aspect, 0.1, 800);

      // Calculate barrel position from screen coords (85% from left, 85% from top = bottom-right with 15% margin)
      const worldPos = screenToWorld(0.85, 0.85, proj, viewMatrix);
      barrelPos[0] = worldPos[0];
      barrelPos[1] = worldPos[1];
      barrelPos[2] = worldPos[2];

      const barrelWaveHeight = barrelWaveHeightAt(barrelPos[0], barrelPos[2], t);

      // Spawn ripples constantly; modulate strength by bobbing velocity.
      if (lastRenderT !== null) {
        const dt = Math.max(0.001, t - lastRenderT);
        const vel = (barrelWaveHeight - prevBarrelWaveHeight) / dt;
        const v = Math.abs(vel);

        if ((t - lastRippleSpawn) > rippleInterval) {
          spawnRipple(t, 1.0);
          lastRippleSpawn = t;
        }
      } else {
        spawnRipple(t, 1.0);
        lastRippleSpawn = t;
      }
      lastRenderT = t;
      prevBarrelWaveHeight = barrelWaveHeight;

      barrelCenter[0] = barrelPos[0];
      barrelCenter[1] = barrelWaveHeight - 0.8 + 1.12;
      barrelCenter[2] = barrelPos[2];

      // Draw background
      gl.useProgram(bgProgram);
    gl.bindBuffer(gl.ARRAY_BUFFER, bgBuffer);
    const bgPos = gl.getAttribLocation(bgProgram, 'a_position');
    gl.enableVertexAttribArray(bgPos);
    gl.vertexAttribPointer(bgPos, 2, gl.FLOAT, false, 0, 0);
    gl.depthMask(false);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    gl.depthMask(true);

    // Draw ocean
    gl.useProgram(oceanProgram);
    
    gl.bindBuffer(gl.ARRAY_BUFFER, oceanVertBuffer);
    const posLoc = gl.getAttribLocation(oceanProgram, 'a_position');
    gl.enableVertexAttribArray(posLoc);
    gl.vertexAttribPointer(posLoc, 3, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ARRAY_BUFFER, oceanUvBuffer);
    const uvLoc = gl.getAttribLocation(oceanProgram, 'a_uv');
    gl.enableVertexAttribArray(uvLoc);
    gl.vertexAttribPointer(uvLoc, 2, gl.FLOAT, false, 0, 0);

      gl.uniformMatrix4fv(gl.getUniformLocation(oceanProgram, 'uProjection'), false, proj);
      gl.uniformMatrix4fv(gl.getUniformLocation(oceanProgram, 'uView'), false, viewMatrix);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uTime'), t);
      gl.uniform3fv(gl.getUniformLocation(oceanProgram, 'uLightDir'), lightDir);
      gl.uniform3fv(gl.getUniformLocation(oceanProgram, 'uBarrelCenter'), barrelCenter);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uBarrelRadius'), 0.8);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uBarrelHalfHeight'), 1.12);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uShadowDebug'), 0.0);
      gl.uniform1fv(gl.getUniformLocation(oceanProgram, 'uRippleTimes[0]'), rippleTimes);
      gl.uniform1fv(gl.getUniformLocation(oceanProgram, 'uRippleAmps[0]'), rippleAmps);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uRippleSpeed'), rippleSpeed);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uRippleMaxAge'), rippleMaxAge);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uRippleWidth'), 0.085);
      gl.uniform1f(gl.getUniformLocation(oceanProgram, 'uRippleStrength'), 1.9);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(gl.getUniformLocation(oceanProgram, 'uMap'), 0);

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, oceanIndexBuffer);
    gl.drawElements(gl.TRIANGLES, oceanIndices.length, indexType, 0);

    // Draw barrel
    gl.useProgram(barrelProgram);
    
    gl.bindBuffer(gl.ARRAY_BUFFER, barrelVertBuffer);
    const barrelPosLoc = gl.getAttribLocation(barrelProgram, 'a_position');
    gl.enableVertexAttribArray(barrelPosLoc);
    gl.vertexAttribPointer(barrelPosLoc, 3, gl.FLOAT, false, 0, 0);

    gl.bindBuffer(gl.ARRAY_BUFFER, barrelNormalBuffer);
    const barrelNormLoc = gl.getAttribLocation(barrelProgram, 'a_normal');
    gl.enableVertexAttribArray(barrelNormLoc);
    gl.vertexAttribPointer(barrelNormLoc, 3, gl.FLOAT, false, 0, 0);

      gl.uniformMatrix4fv(gl.getUniformLocation(barrelProgram, 'uProjection'), false, proj);
      gl.uniformMatrix4fv(gl.getUniformLocation(barrelProgram, 'uView'), false, viewMatrix);
      gl.uniform1f(gl.getUniformLocation(barrelProgram, 'uTime'), t);
      gl.uniform3fv(gl.getUniformLocation(barrelProgram, 'uBarrelPos'), barrelPos);
      gl.uniform3fv(gl.getUniformLocation(barrelProgram, 'uLightDir'), lightDir);
      gl.uniform3fv(gl.getUniformLocation(barrelProgram, 'uCameraPos'), cameraEye);

      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, barrelIndexBuffer);
      gl.drawElements(gl.TRIANGLES, barrelIndices.length, gl.UNSIGNED_SHORT, 0);

    requestAnimationFrame(render);
  }
  requestAnimationFrame(render);
})();

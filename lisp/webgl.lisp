

(defvar vertex-shader-source "
precision mediump float;

uniform mat4 modelView;
uniform mat4 model;

attribute vec3 vp;
varying vec3 wp;
varying vec3 eye_dir;

void main() {
    gl_Position = modelView * vec4(vp, 1.0);
    wp = (model * vec4(vp, 1.0)).xyz;
}
"

)

(defvar fragment-shader-source "
precision mediump float;

uniform vec4 color;
uniform sampler2D tex1;
uniform vec3 cameraPosition;
varying vec2 uv2;
varying vec3 eye_dir;

void main() {
    gl_FragColor = color;
}

"

)

(println document)

(defun get-element-by-id (item id)
    (item.getElementById id)
)

(defun get-context (item contextname)
    (item.getContext contextname)
)

(defvar webgl-canvas (get-element-by-id document "webgl-canvas"))
(defvar gl (get-context webgl-canvas "webgl"))
(println gl)
(assert gl)

(defvar vertexshader (gl.createShader gl.VERTEX_SHADER))
(gl.shaderSource vertexshader vertex-shader-source)
(gl.compileShader vertexshader)

(defvar fragmentshader (gl.createShader gl.FRAGMENT_SHADER))
(gl.shaderSource fragmentshader fragment-shader-source)
(gl.compileShader fragmentshader)

(defvar shader-program (gl.createProgram))
(gl.attachShader shader-program vertexshader)
(gl.attachShader shader-program fragmentshader)
(gl.linkProgram shader-program)
(gl.useProgram shader-program)

(defvar link-status (gl.getProgramParameter shader-program gl.LINK_STATUS))
(if (not link-status)
    (println "Failed to link shader program")
)
(println shader-program)

(when false 
(defvar vertices '(0 1 -1 -1 1 -1))

(defvar vertex-buffer (gl.createBuffer))
(gl.bindBuffer gl.ARRAY_BUFFER vertex-buffer)
(gl.bufferData gl.ARRAY_BUFFER (float32-array vertices) gl.STATIC_DRAW)

(defvar position-attribute-location (gl.getAttribLocation shader-program "a_position"))
(gl.enableVertexAttribArray position-attribute-location)
(gl.vertexAttribPointer position-attribute-location 2 gl.FLOAT nil 0 0)
)
(defvar animate t)
(defvar time-component 0.0)
(defun animation-loop ()
    (set time-component (+ time-component 0.01))
    ;; lets make some funky clear-color based on time:
    (gl.clearColor (math:sin time-component) (math:cos time-component) 0.0 1.0)
    
    (gl.clear gl.COLOR_BUFFER_BIT)
    (println "animation loop")
    (when animate 
        (requestAnimationFrame animation-loop)
    )
)

(animation-loop)

;(gl.drawArrays gl.TRIANGLES 0 3)
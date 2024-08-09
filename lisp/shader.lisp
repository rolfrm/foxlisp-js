(defvar shader::vertex-shader-source "
precision highp float;

uniform mat4 view;
uniform mat4 camera;
uniform mat4 model;

attribute vec3 vertexes;
attribute vec3 vertexColor;

varying  vec3 vertColor;
varying float depth;
void main() {
    vec4 p = view * camera * model * vec4(vertexes, 1.0);
    vec4 p2 = camera * model * vec4(vertexes, 1.0);
    
    gl_Position = p;
    depth = p2.z / p2.w;
    vertColor = vertexColor;

}
"

)

(defvar shader::fragment-shader-source "
precision highp float;
uniform vec4 color;
varying vec3 vertColor;
varying float depth;
void main() {
    vec3 fog = vec3(1.0, 1.0, 1.0);
    float fogAmount = 0.0;
    float fogOffset = 0.5;
    float depth2 = gl_FragCoord.z / gl_FragCoord.w;
    if(depth2 > fogOffset){
        fogAmount = (depth2 - fogOffset) * 0.003;
        if(fogAmount > 1.0){
          fogAmount  = 1.0;
        }
    }
    gl_FragColor = vec4(color.xyz * vertColor * (1.0 - fogAmount) + fog * fogAmount,1.0);
}

")


(defun shader:new (vertexShaderCode fragmentShaderCode)
    (let ((vertexShader (gl.createShader gl.VERTEX_SHADER))
            (fragmentShader (gl.createShader gl.FRAGMENT_SHADER))
            (program (gl.createProgram)))
        (gl.shaderSource vertexShader vertexShaderCode)
        (gl.compileShader vertexShader)
        (gl.shaderSource fragmentShader fragmentShaderCode)
        (gl.compileShader fragmentShader)
        (gl.attachShader program vertexShader)
        (gl.attachShader program fragmentShader)
        (gl.linkProgram program)
        (let ((link-status (gl.getProgramParameter program gl.LINK_STATUS)))
            (println link-status)
            (assert link-status))
        (let ((program-object (list :program program)))
            (gl.useProgram program)
            (set program-object.program program)
            (set program-object.vertexes (gl.getAttribLocation program "vertexes"))
            (set program-object.color (gl.getUniformLocation program "color"))
				(set program-object.size (gl.getAttribLocation program "size"))
            (set program-object.model (gl.getUniformLocation program "model"))
            (set program-object.camera (gl.getUniformLocation program "camera"))
            (set program-object.view (gl.getUniformLocation program "view"))
            (set program-object.vertexColor (gl.getAttribLocation program "vertexColor"))
				(shader:set-camera program-object (mat4:identity))
            program-object
        )))

(defun shader:use (shader) 
    (gl.useProgram shader.program)
    (gl.enableVertexAttribArray shader.vertexes)
    (gl.vertexAttribPointer shader.vertexes 3 gl.FLOAT false 0 0)
	 (gl.enableVertexAttribArray shader.vertexColor)
    (gl.vertexAttribPointer shader.vertexColor 3 gl.FLOAT false 0 0)
	 (set shader.current-color (list -1 -1 -1 -1))
	 (when (> shader.size -1)
		
		;(gl.enableVertexAttribArray shader.size)
		;(gl.vertexAttribPointer shader.size 1 gl.FLOAT false 0 0)
		))

(defun shader::normalize-color(c)
  (c.toFixed 2))

(defun shader:set-color (shader r g b a)
  (gl.uniform4f shader.color r g b a))

(defun shader:set-model (shader model) (gl.uniformMatrix4fv shader.model false model))
(defun shader:set-view (shader view-matrix) (gl.uniformMatrix4fv shader.view false view-matrix))
(defun shader:set-camera (shader camera-matrix) (gl.uniformMatrix4fv shader.camera false camera-matrix))

(defvar shader::default nil)

(defun shader:get-default()
  (if shader::default
      shader::default
      (set shader::default (shader:new shader::vertex-shader-source shader::fragment-shader-source)))
  )

(defvar shader::vertex-shader-source-sdf "
precision highp float;

uniform mat4 view;
uniform mat4 model;

attribute vec3 vertexes;
attribute vec3 vertexColor;
attribute float size;
varying vec3 c;
void main() {
    c = vertexColor;
    vec4 p = view * model * vec4(vertexes, 1.0);
    vec4 p2 = model * vec4(vertexes, 1.0);
    if(p.w > 0.0){
       gl_PointSize = max(2.0, 550.0 * size / p.w) ;
    }else{
       gl_PointSize = 20.0;
    }
    gl_Position = p;
}
"

)

(defvar shader::fragment-shader-source-sdf "
precision highp float;

varying vec3 c;
void main() {
    if(length(gl_PointCoord-vec2(0.5)) > 0.5)
        discard;
    
    gl_FragColor = vec4(c,1.0);
}

")

(defvar shader::sdf nil)

(defun shader:get-sdf()
  
    (if shader::sdf
        shader::sdf
        (set shader::sdf (shader:new shader::vertex-shader-source-sdf shader::fragment-shader-source-sdf)))
)

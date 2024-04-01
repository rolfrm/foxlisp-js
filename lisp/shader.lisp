(defvar shader::vertex-shader-source "
precision mediump float;

uniform mat4 modelView;
uniform mat4 model;

attribute vec3 vertexes;
attribute vec3 vertexColor;

varying vec3 vertColor;
void main() {
    gl_Position = modelView * vec4(vertexes, 1.0);
    vertColor = vertexColor;
}
"

)

(defvar shader::fragment-shader-source "
precision mediump float;
uniform vec4 color;
varying vec3 vertColor;
void main() {
    gl_FragColor = vec4(vertColor,1) * color;
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
            (set program-object.model (gl.getUniformLocation program "model"))
            (set program-object.modelView (gl.getUniformLocation program "modelView"))
            (set program-object.vertexColor (gl.getAttribLocation program "vertexColor"))
            program-object
        )))

(defun shader:use (shader) 
    (gl.useProgram shader.program)
    (gl.enableVertexAttribArray shader.vertexes)
    (gl.vertexAttribPointer shader.vertexes 3 gl.FLOAT false 0 0)
    (gl.enableVertexAttribArray shader.vertexColor)
    (gl.vertexAttribPointer shader.vertexColor 3 gl.FLOAT false 0 0))
    

(defun shader:set-color (shader r g b a) (gl.uniform4f shader.color r g b a))
(defun shader:set-model (shader model) (gl.uniformMatrix4fv shader.model false model))
(defun shader:set-model-view (shader modelView) (gl.uniformMatrix4fv shader.modelView false modelView))

(defvar shader::default nil)

(defun shader:get-default()
    (if shader::default
        shader::default
        (set shader::default (shader:new shader::vertex-shader-source shader::fragment-shader-source)))
)


          
          
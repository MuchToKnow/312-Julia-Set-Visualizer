layout(location = 0) in vec4 vPosition
layout(location = 1) in vec2 coords;
out vec2 fragmentCoord;

void main() {
    gl_Position = vPosition
    fragCoord = coords
}

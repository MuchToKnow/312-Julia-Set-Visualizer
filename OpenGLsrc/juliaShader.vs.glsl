#version 330

layout (location = 0) in vec4 vPosition;
out vec2 fragCoord;

void main() {
    gl_Position = vPosition;
    fragCoord = vPosition.xy;
}

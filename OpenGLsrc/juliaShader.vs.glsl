#version 330

layout (location = 0) in vec4 vPosition;
out vec2 fragCoord;
out float time;

void main() {
	time = 1;
    gl_Position = vPosition;
    fragCoord = vPosition.xy;
}

/*
 * C helper for loading OpenGL functions via GLAD and GLFW.
 * This simplifies the Fortran binding since we can't easily pass
 * glfwGetProcAddress as a function pointer from Fortran.
 *
 * Also provides wrapper functions for OpenGL calls, since GLAD uses
 * function pointers that can't be directly called from Fortran bindings.
 */

#include <glad/gl.h>
#include <GLFW/glfw3.h>

/* Load OpenGL functions using GLFW's loader */
int fortty_load_gl(void) {
    return gladLoadGL((GLADloadfunc) glfwGetProcAddress);
}

/* OpenGL wrapper functions for Fortran */
void fortty_glViewport(int x, int y, int width, int height) {
    glViewport(x, y, width, height);
}

void fortty_glClear(unsigned int mask) {
    glClear(mask);
}

void fortty_glClearColor(float red, float green, float blue, float alpha) {
    glClearColor(red, green, blue, alpha);
}

/* ============ Texture functions ============ */

void fortty_glGenTextures(int n, unsigned int *textures) {
    glGenTextures(n, textures);
}

void fortty_glDeleteTextures(int n, unsigned int *textures) {
    glDeleteTextures(n, textures);
}

void fortty_glBindTexture(unsigned int target, unsigned int texture) {
    glBindTexture(target, texture);
}

void fortty_glTexImage2D(unsigned int target, int level, int internalformat,
                         int width, int height, int border,
                         unsigned int format, unsigned int type, const void *data) {
    glTexImage2D(target, level, internalformat, width, height, border, format, type, data);
}

void fortty_glTexSubImage2D(unsigned int target, int level,
                            int xoffset, int yoffset, int width, int height,
                            unsigned int format, unsigned int type, const void *data) {
    glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, data);
}

void fortty_glTexParameteri(unsigned int target, unsigned int pname, int param) {
    glTexParameteri(target, pname, param);
}

void fortty_glPixelStorei(unsigned int pname, int param) {
    glPixelStorei(pname, param);
}

void fortty_glActiveTexture(unsigned int texture) {
    glActiveTexture(texture);
}

/* ============ Shader functions ============ */

unsigned int fortty_glCreateShader(unsigned int type) {
    return glCreateShader(type);
}

void fortty_glDeleteShader(unsigned int shader) {
    glDeleteShader(shader);
}

void fortty_glShaderSource(unsigned int shader, const char *source) {
    const char *sources[1] = { source };
    glShaderSource(shader, 1, sources, NULL);
}

void fortty_glCompileShader(unsigned int shader) {
    glCompileShader(shader);
}

int fortty_glGetShaderCompileStatus(unsigned int shader) {
    int success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    return success;
}

void fortty_glGetShaderInfoLog(unsigned int shader, char *log, int maxlen) {
    glGetShaderInfoLog(shader, maxlen, NULL, log);
}

unsigned int fortty_glCreateProgram(void) {
    return glCreateProgram();
}

void fortty_glDeleteProgram(unsigned int program) {
    glDeleteProgram(program);
}

void fortty_glAttachShader(unsigned int program, unsigned int shader) {
    glAttachShader(program, shader);
}

void fortty_glLinkProgram(unsigned int program) {
    glLinkProgram(program);
}

int fortty_glGetProgramLinkStatus(unsigned int program) {
    int success;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    return success;
}

void fortty_glGetProgramInfoLog(unsigned int program, char *log, int maxlen) {
    glGetProgramInfoLog(program, maxlen, NULL, log);
}

void fortty_glUseProgram(unsigned int program) {
    glUseProgram(program);
}

int fortty_glGetUniformLocation(unsigned int program, const char *name) {
    return glGetUniformLocation(program, name);
}

void fortty_glUniform1i(int location, int value) {
    glUniform1i(location, value);
}

void fortty_glUniform1f(int location, float value) {
    glUniform1f(location, value);
}

void fortty_glUniform3f(int location, float v0, float v1, float v2) {
    glUniform3f(location, v0, v1, v2);
}

void fortty_glUniform4f(int location, float v0, float v1, float v2, float v3) {
    glUniform4f(location, v0, v1, v2, v3);
}

void fortty_glUniformMatrix4fv(int location, int count, int transpose, const float *value) {
    glUniformMatrix4fv(location, count, transpose, value);
}

/* ============ VAO/VBO functions ============ */

void fortty_glGenVertexArrays(int n, unsigned int *arrays) {
    glGenVertexArrays(n, arrays);
}

void fortty_glDeleteVertexArrays(int n, unsigned int *arrays) {
    glDeleteVertexArrays(n, arrays);
}

void fortty_glBindVertexArray(unsigned int array) {
    glBindVertexArray(array);
}

void fortty_glGenBuffers(int n, unsigned int *buffers) {
    glGenBuffers(n, buffers);
}

void fortty_glDeleteBuffers(int n, unsigned int *buffers) {
    glDeleteBuffers(n, buffers);
}

void fortty_glBindBuffer(unsigned int target, unsigned int buffer) {
    glBindBuffer(target, buffer);
}

void fortty_glBufferData(unsigned int target, size_t size, const void *data, unsigned int usage) {
    glBufferData(target, size, data, usage);
}

void fortty_glBufferSubData(unsigned int target, size_t offset, size_t size, const void *data) {
    glBufferSubData(target, offset, size, data);
}

void fortty_glBufferSubData_floats(unsigned int target, size_t offset, size_t size, const float *data) {
    glBufferSubData(target, offset, size, data);
}

void fortty_glVertexAttribPointer(unsigned int index, int size, unsigned int type,
                                   int normalized, int stride, size_t offset) {
    glVertexAttribPointer(index, size, type, normalized, stride, (void*)offset);
}

void fortty_glEnableVertexAttribArray(unsigned int index) {
    glEnableVertexAttribArray(index);
}

void fortty_glDisableVertexAttribArray(unsigned int index) {
    glDisableVertexAttribArray(index);
}

/* ============ Drawing functions ============ */

void fortty_glDrawArrays(unsigned int mode, int first, int count) {
    glDrawArrays(mode, first, count);
}

/* ============ State functions ============ */

void fortty_glEnable(unsigned int cap) {
    glEnable(cap);
}

void fortty_glDisable(unsigned int cap) {
    glDisable(cap);
}

void fortty_glBlendFunc(unsigned int sfactor, unsigned int dfactor) {
    glBlendFunc(sfactor, dfactor);
}

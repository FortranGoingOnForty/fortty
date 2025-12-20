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

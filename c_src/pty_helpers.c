/*
 * PTY helper functions for fortty
 * Wraps POSIX PTY operations for Fortran binding
 */

#define _XOPEN_SOURCE 600

/* Platform-specific PTY header */
#if defined(__APPLE__)
#include <util.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
#include <libutil.h>
#else
#include <pty.h>
#endif

#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Store child PID for status checking */
static pid_t child_pid = -1;

/*
 * Fork a shell process with PTY
 * Returns master fd on success, -1 on error
 */
int fortty_pty_fork(const char *shell, int rows, int cols) {
    int master_fd;
    struct winsize ws;

    /* Set initial window size */
    ws.ws_row = rows;
    ws.ws_col = cols;
    ws.ws_xpixel = 0;
    ws.ws_ypixel = 0;

    /* forkpty does: openpty + fork + login_tty */
    child_pid = forkpty(&master_fd, NULL, NULL, &ws);

    if (child_pid < 0) {
        perror("forkpty");
        return -1;
    }

    if (child_pid == 0) {
        /* Child process */

        /* Set environment for terminal */
        setenv("TERM", "xterm-256color", 1);
        setenv("COLORTERM", "truecolor", 1);

        /* Determine shell to use */
        const char *sh = shell;
        if (!sh || sh[0] == '\0') {
            sh = getenv("SHELL");
            if (!sh) {
                sh = "/bin/sh";
            }
        }

        /* Execute shell as interactive shell (not login) */
        execlp(sh, sh, (char *)NULL);

        /* If exec fails */
        perror("execlp");
        _exit(127);
    }

    /* Parent process */

    /* Set non-blocking mode on master */
    int flags = fcntl(master_fd, F_GETFL, 0);
    if (flags != -1) {
        fcntl(master_fd, F_SETFL, flags | O_NONBLOCK);
    }

    return master_fd;
}

/*
 * Set PTY window size
 * Returns 0 on success, -1 on error
 */
int fortty_pty_set_size(int master_fd, int rows, int cols) {
    struct winsize ws;
    ws.ws_row = rows;
    ws.ws_col = cols;
    ws.ws_xpixel = 0;
    ws.ws_ypixel = 0;

    if (ioctl(master_fd, TIOCSWINSZ, &ws) < 0) {
        perror("ioctl TIOCSWINSZ");
        return -1;
    }

    return 0;
}

/*
 * Read from PTY (non-blocking)
 * Returns: bytes read (>0), 0 if would block, -1 on error/EOF
 */
int fortty_pty_read(int fd, char *buf, int count) {
    ssize_t n = read(fd, buf, count);

    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;  /* No data available */
        }
        return -1;  /* Actual error */
    }

    if (n == 0) {
        return -1;  /* EOF - child closed PTY */
    }

    return (int)n;
}

/*
 * Write to PTY
 * Returns: bytes written, -1 on error
 */
int fortty_pty_write(int fd, const char *buf, int count) {
    ssize_t n = write(fd, buf, count);
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return 0;  /* Would block, try again later */
        }
        perror("write to pty");
        return -1;
    }
    return (int)n;
}

/*
 * Close PTY and wait for child
 */
void fortty_pty_close(int master_fd) {
    if (master_fd >= 0) {
        close(master_fd);
    }

    if (child_pid > 0) {
        /* Send SIGHUP to child (standard terminal close behavior) */
        kill(child_pid, SIGHUP);

        /* Wait for child to exit (with timeout via WNOHANG loop) */
        int status;
        int attempts = 0;
        while (waitpid(child_pid, &status, WNOHANG) == 0 && attempts < 100) {
            usleep(10000);  /* 10ms */
            attempts++;
        }

        /* Force kill if still running */
        if (attempts >= 100) {
            kill(child_pid, SIGKILL);
            waitpid(child_pid, &status, 0);
        }

        child_pid = -1;
    }
}

/*
 * Check if child process is still alive
 * Returns: 1 if alive, 0 if dead/not started
 */
int fortty_pty_child_alive(void) {
    if (child_pid <= 0) {
        return 0;
    }

    int status;
    pid_t result = waitpid(child_pid, &status, WNOHANG);

    if (result == 0) {
        /* Child still running */
        return 1;
    }

    if (result == child_pid) {
        /* Child exited */
        child_pid = -1;
        return 0;
    }

    /* Error (treat as dead) */
    return 0;
}

/*
 * Get the child PID (for debugging/signals)
 */
int fortty_pty_get_child_pid(void) {
    return (int)child_pid;
}

/*
 * Window blur stub - macOS blur requires Cocoa framework which has
 * CMake integration issues with Fortran. This is a no-op placeholder.
 * TODO: Implement with NSVisualEffectView when build system supports it.
 */
void fortty_set_window_blur(void* window, int enable) {
    (void)window;
    (void)enable;
}

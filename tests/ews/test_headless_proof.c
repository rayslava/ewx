/* Proof-of-concept: headless Wayland backend works with zero dependencies */
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <cmocka.h>
#include <wayland-server-core.h>

#define WLR_USE_UNSTABLE
#include <wlr/backend.h>
#include <wlr/backend/headless.h>
#include <wlr/render/wlr_renderer.h>

/* Minimal test - just create/destroy headless server */
static void test_headless_backend_creation(void **state) {
    (void)state;
    
    /* Create Wayland display */
    struct wl_display *display = wl_display_create();
    assert_non_null(display);
    
    /* Create headless backend */
    struct wlr_backend *backend = wlr_headless_backend_create(display);
    assert_non_null(backend);
    
    /* Create renderer */
    struct wlr_renderer *renderer = wlr_renderer_autocreate(backend);
    assert_non_null(renderer);
    
    /* Clean up */
    wlr_renderer_destroy(renderer);
    wlr_backend_destroy(backend);
    wl_display_destroy(display);
}

/* Test that headless backend can be started/stopped */
static void test_headless_backend_lifecycle(void **state) {
    (void)state;
    
    struct wl_display *display = wl_display_create();
    struct wlr_backend *backend = wlr_headless_backend_create(display);
    
    /* Test backend start/stop */
    bool started = wlr_backend_start(backend);
    assert_true(started);
    
    /* Clean up */
    wlr_backend_destroy(backend);
    wl_display_destroy(display);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_headless_backend_creation),
        cmocka_unit_test(test_headless_backend_lifecycle),
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL);
}
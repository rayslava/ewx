#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <cmocka.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xdg_shell.h>

#include "ews_types.h"
#include "ews_internal.h"

/* Minimal headless server setup for testing */
static struct ews_server *setup_headless_server(void) {
    struct ews_server *server = calloc(1, sizeof(*server));
    
    /* Create Wayland display */
    server->wl_display = wl_display_create();
    assert_non_null(server->wl_display);
    
    /* Use headless backend for testing */
    server->backend = wlr_headless_backend_create(server->wl_display);
    if (!server->backend) {
        wl_display_destroy(server->wl_display);
        free(server);
        return NULL;
    }
    
    /* Create renderer */
    server->renderer = wlr_renderer_autocreate(server->backend);
    if (!server->renderer) {
        wlr_backend_destroy(server->backend);
        wl_display_destroy(server->wl_display);
        free(server);
        return NULL;
    }
    
    wlr_renderer_init_wl_display(server->renderer, server->wl_display);
    
    /* Create basic compositor infrastructure */
    wlr_compositor_create(server->wl_display, 5, server->renderer);
    server->output_layout = wlr_output_layout_create();
    server->scene = wlr_scene_create();
    wlr_scene_attach_output_layout(server->scene, server->output_layout);
    
    /* Initialize lists */
    wl_list_init(&server->outputs);
    wl_list_init(&server->surfaces);
    wl_list_init(&server->keyboards);
    
    /* Create seat for input testing */
    server->seat = wlr_seat_create(server->wl_display, "test-seat");
    
    return server;
}

static void teardown_headless_server(struct ews_server *server) {
    if (!server) return;
    
    wl_display_destroy_clients(server->wl_display);
    wl_display_destroy(server->wl_display);
    free(server);
}

/* Test actual surface management with real Wayland structures */
static void test_surface_management_integration(void **state) {
    (void)state;
    
    struct ews_server *server = setup_headless_server();
    assert_non_null(server);
    
    /* Create XDG shell for surface testing */
    server->xdg_shell = wlr_xdg_shell_create(server->wl_display, 3);
    assert_non_null(server->xdg_shell);
    
    /* Test that we can set up surface event handlers */
    server->new_xdg_surface.notify = server_new_xdg_surface;
    wl_signal_add(&server->xdg_shell->events.new_surface, &server->new_xdg_surface);
    
    /* Test focus_surface with real seat */
    focus_surface(NULL, NULL); /* Should handle NULL gracefully */
    
    teardown_headless_server(server);
}

/* Test output management with virtual outputs */
static void test_output_management_integration(void **state) {
    (void)state;
    
    struct ews_server *server = setup_headless_server();
    assert_non_null(server);
    
    /* Create a headless output */
    struct wlr_output *output = wlr_headless_backend_create_output(server->backend);
    assert_non_null(output);
    
    /* Test our find_output_by_index function with real output */
    struct ews_output test_output = {0};
    test_output.wlr_output = output;
    wl_list_init(&test_output.link);
    wl_list_insert(&server->outputs, &test_output.link);
    
    /* Now test with populated list */
    struct ews_output *found = find_output_by_index(server, 0);
    assert_ptr_equal(found, &test_output);
    
    found = find_output_by_index(server, 1);
    assert_null(found);
    
    teardown_headless_server(server);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_surface_management_integration),
        cmocka_unit_test(test_output_management_integration),
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL);
}
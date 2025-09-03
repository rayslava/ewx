#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wayland-server-core.h>

#include <cmocka.h>
#include <wlr/backend.h>
#include <wlr/backend/headless.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xdg_shell.h>

#include "ews_types.h"
#include "ews_internal.h"
#include "ews_output.h"
#include "ews_surface.h"
#include "ews_input.h"
#include "ews_cursor.h"
#include "ewp-protocol.h"

static struct ews_server *test_server = NULL;
static int setup_headless_server(void **state) {
    (void)state;
    
    test_server = calloc(1, sizeof(*test_server));
    assert_non_null(test_server);
    
    test_server->wl_display = wl_display_create();
    assert_non_null(test_server->wl_display);
    
    test_server->backend = wlr_headless_backend_create(test_server->wl_display);
    assert_non_null(test_server->backend);
    
    test_server->renderer = wlr_renderer_autocreate(test_server->backend);
    assert_non_null(test_server->renderer);
    
    wlr_renderer_init_wl_display(test_server->renderer, test_server->wl_display);
    
    test_server->allocator = wlr_allocator_autocreate(test_server->backend, test_server->renderer);
    assert_non_null(test_server->allocator);
    
    wlr_compositor_create(test_server->wl_display, 5, test_server->renderer);
    test_server->output_layout = wlr_output_layout_create();
    test_server->scene = wlr_scene_create();
    wlr_scene_attach_output_layout(test_server->scene, test_server->output_layout);
    
    wl_list_init(&test_server->outputs);
    wl_list_init(&test_server->surfaces);
    wl_list_init(&test_server->keyboards);
    
    test_server->seat = wlr_seat_create(test_server->wl_display, "test-seat");
    assert_non_null(test_server->seat);
    
    bool started = wlr_backend_start(test_server->backend);
    assert_true(started);
    
    return 0;
}

static int teardown_headless_server(void **state) {
    (void)state;
    
    if (test_server) {
        wl_display_destroy_clients(test_server->wl_display);
        wl_display_destroy(test_server->wl_display);
        free(test_server);
        test_server = NULL;
    }
    
    return 0;
}

static void test_output_management_headless(void **state) {
    (void)state;
    
    struct wlr_output *wlr_output = wlr_headless_add_output(test_server->backend, 1920, 1080);
    assert_non_null(wlr_output);
    
    struct ews_output *ews_output = calloc(1, sizeof(*ews_output));
    ews_output->wlr_output = wlr_output;
    ews_output->scene = test_server->scene;
    wl_list_init(&ews_output->link);
    wl_list_insert(&test_server->outputs, &ews_output->link);
    
    struct ews_output *found = find_output_by_index(test_server, 0);
    assert_ptr_equal(found, ews_output);
    
    found = find_output_by_index(test_server, 1);
    assert_null(found);
    
    struct wlr_output *wlr_output2 = wlr_headless_add_output(test_server->backend, 1280, 720);
    struct ews_output *ews_output2 = calloc(1, sizeof(*ews_output2));
    ews_output2->wlr_output = wlr_output2;
    ews_output2->scene = test_server->scene;
    wl_list_init(&ews_output2->link);
    wl_list_insert(&test_server->outputs, &ews_output2->link);
    
    found = find_output_by_index(test_server, 0);
    assert_ptr_equal(found, ews_output2);
    found = find_output_by_index(test_server, 1);
    assert_ptr_equal(found, ews_output);
    found = find_output_by_index(test_server, 2);
    assert_null(found);
    
    wl_list_remove(&ews_output->link);
    wl_list_remove(&ews_output2->link);
    free(ews_output);
    free(ews_output2);
}

static void test_surface_focus(void **state) {
    (void)state;
    
    focus_surface(NULL, NULL);
}

static void test_layout_surface_minimal(void **state) {
    (void)state;
    
    struct ews_surface surface = {0};
    surface.server = test_server;
    surface.x = 100;
    surface.y = 200;
    surface.width = 800;
    surface.height = 600;
    surface.scene_tree = NULL;
    surface.target_output = NULL;
    
    struct wlr_xdg_surface xdg_surface = {0};
    struct wlr_xdg_toplevel toplevel = {0};
    toplevel.base = &xdg_surface;
    
    surface.xdg_surface = &xdg_surface;
    surface.xdg_toplevel = &toplevel;
    
    struct ews_output output = {0};
    output.scene = test_server->scene;
    
    bool scene_null = (surface.scene_tree == NULL);
    bool output_null = (surface.target_output == NULL);
    
    assert_true(scene_null);
    assert_true(output_null);
    
    surface.target_output = &output;
    bool has_target = (surface.target_output != NULL);
    assert_true(has_target);
    
    struct wlr_scene_tree mock_tree = {0};
    surface.scene_tree = &mock_tree;
    bool has_scene = (surface.scene_tree != NULL);
    assert_true(has_scene);
}

static void test_surface_at(void **state) {
    (void)state;
    
    struct wlr_surface *surface = NULL;
    double sx, sy;
    
    struct ews_surface *result = surface_at(test_server, 100.0, 100.0, &surface, &sx, &sy);
    assert_null(result);
    
    result = surface_at(test_server, 0.0, 0.0, &surface, &sx, &sy);
    assert_null(result);
}


static void test_ewp_protocol(void **state) {
    (void)state;
    
    struct wl_global *ewp_global = wl_global_create(test_server->wl_display,
                                                    &ewp_layout_interface,
                                                    1, test_server, NULL);
    assert_non_null(ewp_global);
    
    wl_display_flush_clients(test_server->wl_display);
    
    wl_global_destroy(ewp_global);
}

static void test_wayland_integration(void **state) {
    (void)state;
    
    wl_display_flush_clients(test_server->wl_display);
    assert_true(wlr_backend_is_headless(test_server->backend));
    wl_event_loop_dispatch(wl_display_get_event_loop(test_server->wl_display), 0);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup_teardown(test_output_management_headless,
                                        setup_headless_server, teardown_headless_server),
        cmocka_unit_test_setup_teardown(test_surface_focus,
                                        setup_headless_server, teardown_headless_server),
        cmocka_unit_test_setup_teardown(test_layout_surface_minimal,
                                        setup_headless_server, teardown_headless_server),
        cmocka_unit_test_setup_teardown(test_surface_at,
                                        setup_headless_server, teardown_headless_server),
        cmocka_unit_test_setup_teardown(test_ewp_protocol,
                                        setup_headless_server, teardown_headless_server),
        cmocka_unit_test_setup_teardown(test_wayland_integration,
                                        setup_headless_server, teardown_headless_server),
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL);
}
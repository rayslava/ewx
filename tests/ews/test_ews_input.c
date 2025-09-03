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
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_virtual_keyboard_v1.h>
#include <wlr/types/wlr_virtual_pointer_v1.h>

#include "ews_types.h"
#include "ews_internal.h"
#include "ews_input.h"

static struct ews_server *test_server = NULL;

static int setup_input_server(void **state) {
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

static int teardown_input_server(void **state) {
    (void)state;
    
    if (test_server) {
        wl_display_destroy_clients(test_server->wl_display);
        wl_display_destroy(test_server->wl_display);
        free(test_server);
        test_server = NULL;
    }
    
    return 0;
}

static void test_keyboard_management(void **state) {
    (void)state;
    
    struct wlr_virtual_keyboard_manager_v1 *keyboard_mgr = 
        wlr_virtual_keyboard_manager_v1_create(test_server->wl_display);
    assert_non_null(keyboard_mgr);
    
    assert_true(wl_list_empty(&test_server->keyboards));
    
    struct ews_keyboard *test_keyboard = calloc(1, sizeof(*test_keyboard));
    test_keyboard->server = test_server;
    wl_list_init(&test_keyboard->link);
    
    wl_list_insert(&test_server->keyboards, &test_keyboard->link);
    assert_false(wl_list_empty(&test_server->keyboards));
    
    wl_list_remove(&test_keyboard->link);
    free(test_keyboard);
}


int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup_teardown(test_keyboard_management,
                                        setup_input_server, teardown_input_server),
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL);
}
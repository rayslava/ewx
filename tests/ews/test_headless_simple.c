/* Minimal example showing how little code is needed for headless testing */
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

#include "ews_types.h"
#include "ews_internal.h"

/* Minimal headless setup - just 30 lines */
static struct ews_server *create_test_server(void) {
    struct ews_server *server = calloc(1, sizeof(*server));
    
    server->wl_display = wl_display_create();
    server->backend = wlr_headless_backend_create(server->wl_display);
    server->renderer = wlr_renderer_autocreate(server->backend);
    
    if (!server->wl_display || !server->backend || !server->renderer) {
        if (server->renderer) wlr_renderer_destroy(server->renderer);
        if (server->backend) wlr_backend_destroy(server->backend);
        if (server->wl_display) wl_display_destroy(server->wl_display);
        free(server);
        return NULL;
    }
    
    wl_list_init(&server->outputs);
    wl_list_init(&server->surfaces);
    return server;
}

static void destroy_test_server(struct ews_server *server) {
    if (!server) return;
    wl_display_destroy(server->wl_display);
    free(server);
}

/* Real integration test - just 10 lines */
static void test_output_management_real(void **state) {
    (void)state;
    
    struct ews_server *server = create_test_server();
    assert_non_null(server);
    
    /* Test with empty outputs */
    assert_null(find_output_by_index(server, 0));
    
    /* Create virtual output */
    struct wlr_output *output = wlr_headless_backend_create_output(server->backend);
    assert_non_null(output);
    
    destroy_test_server(server);
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_output_management_real),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
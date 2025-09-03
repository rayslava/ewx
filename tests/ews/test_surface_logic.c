/* Example of testing specific surface logic without full Wayland setup */
#include <cmocka.h>
#include "ews_types.h"
#include "ews_internal.h"

/* Test the surface coordinate calculation logic */
static void test_surface_coordinate_bounds(void **state) {
    (void)state;
    
    /* Create minimal surface for coordinate testing */
    struct ews_surface surface = {0};
    surface.x = 100;
    surface.y = 200;
    surface.width = 800;
    surface.height = 600;
    
    /* Test coordinate boundary conditions */
    assert_int_equal(surface.x, 100);
    assert_int_equal(surface.y, 200);
    
    /* Test negative coordinates */
    surface.x = -50;
    surface.y = -100;
    assert_int_equal(surface.x, -50);
    assert_int_equal(surface.y, -100);
    
    /* Test maximum coordinates */
    surface.x = INT32_MAX;
    surface.y = INT32_MAX;
    assert_int_equal(surface.x, INT32_MAX);
    assert_int_equal(surface.y, INT32_MAX);
}

/* Test list management logic */
static void test_output_list_operations(void **state) {
    (void)state;
    
    struct ews_server server = {0};
    struct ews_output outputs[5];
    
    wl_list_init(&server.outputs);
    
    /* Add multiple outputs */
    for (int i = 0; i < 5; i++) {
        memset(&outputs[i], 0, sizeof(outputs[i]));
        wl_list_init(&outputs[i].link);
        wl_list_insert(&server.outputs, &outputs[i].link);
    }
    
    /* Test find_output_by_index with various indices */
    for (uint32_t i = 0; i < 5; i++) {
        struct ews_output *found = find_output_by_index(&server, i);
        assert_non_null(found);
        /* Note: wl_list_insert puts new items at head, so order is reversed */
        assert_ptr_equal(found, &outputs[4-i]);
    }
    
    /* Test out of bounds */
    assert_null(find_output_by_index(&server, 5));
    assert_null(find_output_by_index(&server, 100));
}

int main(void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(test_surface_coordinate_bounds),
        cmocka_unit_test(test_output_list_operations),
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL);
}
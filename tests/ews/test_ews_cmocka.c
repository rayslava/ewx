#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <wayland-server-core.h>

#include <cmocka.h>

#include "ews_cursor.h"
#include "ews_ewp.h"
#include "ews_input.h"
#include "ews_internal.h"
#include "ews_output.h"
#include "ews_surface.h"
#include "ews_types.h"

static int setup_test(void **state) {
  (void)state;
  return 0;
}

static int teardown_test(void **state) {
  (void)state;
  return 0;
}

static void test_ews_integration(void **state) { (void)state; }

static void test_focus_surface_null_handling(void **state) {
  (void)state;
  focus_surface(NULL, NULL);
}

static void test_modular_function_access(void **state) {
  (void)state;
  assert_non_null(find_output_by_index);
  assert_non_null(layout_surface);
}

static void test_surface_at_null_server_outputs(void **state) {
  (void)state;
  struct ews_server test_server = {0};
  wl_list_init(&test_server.outputs);
  struct wlr_surface *surface = NULL;
  double sx, sy;
  assert_null(surface_at(&test_server, 100.0, 100.0, &surface, &sx, &sy));
  assert_null(surface);
}

static void test_find_output_by_index_empty_server(void **state) {
  (void)state;
  struct ews_server test_server = {0};
  wl_list_init(&test_server.outputs);
  assert_null(find_output_by_index(&test_server, 0));
  assert_null(find_output_by_index(&test_server, 5));
}

static void test_seat_request_handlers(void **state) {
  (void)state;
  assert_non_null(seat_request_cursor);
  assert_non_null(seat_request_set_selection);
  assert_non_null(server_new_input);
}

static void test_cursor_surface_detection(void **state) {
  (void)state;
  struct ews_server test_server = {0};
  wl_list_init(&test_server.outputs);
  struct wlr_surface *surface = NULL;
  double sx, sy;
  assert_null(surface_at(&test_server, 0.0, 0.0, &surface, &sx, &sy));
  assert_null(surface_at(&test_server, -10.0, -5.0, &surface, &sx, &sy));
  assert_null(surface_at(&test_server, 10000.0, 10000.0, &surface, &sx, &sy));
}

static void test_output_enumeration(void **state) {
  (void)state;
  struct ews_server test_server = {0};
  wl_list_init(&test_server.outputs);
  assert_null(find_output_by_index(&test_server, 0));
  assert_null(find_output_by_index(&test_server, UINT32_MAX));
}

static void test_output_index_with_populated_list(void **state) {
  (void)state;
  struct ews_server test_server = {0};
  struct ews_output output1 = {0}, output2 = {0}, output3 = {0};

  wl_list_init(&test_server.outputs);
  wl_list_init(&output1.link);
  wl_list_init(&output2.link);
  wl_list_init(&output3.link);

  wl_list_insert(&test_server.outputs, &output1.link);
  wl_list_insert(&test_server.outputs, &output2.link);
  wl_list_insert(&test_server.outputs, &output3.link);

  assert_ptr_equal(find_output_by_index(&test_server, 0), &output3);
  assert_ptr_equal(find_output_by_index(&test_server, 1), &output2);
  assert_ptr_equal(find_output_by_index(&test_server, 2), &output1);
  assert_null(find_output_by_index(&test_server, 3));
  assert_null(find_output_by_index(&test_server, 100));
}

int main(void) {
  const struct CMUnitTest tests[] = {
      cmocka_unit_test(test_ews_integration),
      cmocka_unit_test(test_focus_surface_null_handling),
      cmocka_unit_test(test_modular_function_access),
      cmocka_unit_test(test_surface_at_null_server_outputs),
      cmocka_unit_test(test_find_output_by_index_empty_server),
      cmocka_unit_test(test_seat_request_handlers),
      cmocka_unit_test(test_cursor_surface_detection),
      cmocka_unit_test(test_output_enumeration),
      cmocka_unit_test(test_output_index_with_populated_list),
  };

  return cmocka_run_group_tests(tests, setup_test, teardown_test);
}

/* test_ews.c - Unit tests for ews functions using Check framework
 *
 * This file contains unit tests for actual functions from ews.c
 */

#include <check.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Include the internal header to access testable functions */
#include "../../ews/ews_internal.h"

/* ============================================================================
 * TEST CASES FOR EWS INTEGRATION
 * ============================================================================
 */

/* Test suite for basic integration - verify the test framework works */
START_TEST(test_ews_config_struct) {
  struct ews_config config = {0};
  config.valid = true;
  config.show_help = false;
  config.show_version = false;
  config.debug_mode = false;
  config.startup_cmd = NULL;

  ck_assert(config.valid);
  ck_assert(!config.show_help);
  ck_assert(!config.show_version);
  ck_assert(!config.debug_mode);
  ck_assert_ptr_eq(config.startup_cmd, NULL);
}
END_TEST

/* ============================================================================
 * TEST CASES FOR EWS TEST SURFACE OPERATIONS
 * ============================================================================
 */

START_TEST(test_ews_test_surface_contains_point) {
  struct ews_test_surface surface = {100, 100, 200, 150, true, false};

  /* Point inside */
  ck_assert(ews_test_surface_contains_point(&surface, 150, 125));
  ck_assert(ews_test_surface_contains_point(&surface, 299, 249));

  /* Point outside */
  ck_assert(!ews_test_surface_contains_point(&surface, 50, 125));
  ck_assert(!ews_test_surface_contains_point(&surface, 300, 125));

  /* Invisible surface */
  surface.visible = false;
  ck_assert(!ews_test_surface_contains_point(&surface, 150, 125));
}
END_TEST

START_TEST(test_ews_test_surfaces_overlap) {
  struct ews_test_surface s1 = {0, 0, 100, 100, true, false};
  struct ews_test_surface s2 = {50, 50, 100, 100, true, false};
  struct ews_test_surface s3 = {200, 200, 100, 100, true, false};

  /* Overlapping surfaces */
  ck_assert(ews_test_surfaces_overlap(&s1, &s2));
  ck_assert(ews_test_surfaces_overlap(&s2, &s1));

  /* Non-overlapping surfaces */
  ck_assert(!ews_test_surfaces_overlap(&s1, &s3));
  ck_assert(!ews_test_surfaces_overlap(&s3, &s1));

  /* Invisible surface */
  s2.visible = false;
  ck_assert(!ews_test_surfaces_overlap(&s1, &s2));
}
END_TEST

START_TEST(test_ews_test_surface_move) {
  struct ews_test_surface surface = {100, 100, 200, 150, true, false};

  ews_test_surface_move(&surface, 50, -25);
  ck_assert_int_eq(surface.x, 150);
  ck_assert_int_eq(surface.y, 75);

  ews_test_surface_move(&surface, -50, 25);
  ck_assert_int_eq(surface.x, 100);
  ck_assert_int_eq(surface.y, 100);
}
END_TEST

START_TEST(test_ews_test_surface_resize) {
  struct ews_test_surface surface = {100, 100, 200, 150, true, false};

  /* Valid resize */
  ews_test_surface_resize(&surface, 300, 250);
  ck_assert_uint_eq(surface.width, 300);
  ck_assert_uint_eq(surface.height, 250);

  /* Invalid resize (too large) - should not change */
  ews_test_surface_resize(&surface, 10000, 250);
  ck_assert_uint_eq(surface.width, 300);
  ck_assert_uint_eq(surface.height, 250);

  /* Invalid resize (zero) - should not change */
  ews_test_surface_resize(&surface, 0, 250);
  ck_assert_uint_eq(surface.width, 300);
  ck_assert_uint_eq(surface.height, 250);
}
END_TEST

/* ============================================================================
 * TEST SUITE SETUP
 * ============================================================================
 */

static Suite *ews_integration_suite(void) {
  Suite *s;
  TCase *tc_integration;

  s = suite_create("EWS Integration");

  /* Basic integration tests */
  tc_integration = tcase_create("Integration Tests");
  tcase_add_test(tc_integration, test_ews_config_struct);
  suite_add_tcase(s, tc_integration);

  return s;
}

static Suite *ews_test_surface_suite(void) {
  Suite *s;
  TCase *tc_test_surface;

  s = suite_create("EWS Test Surface Operations");

  tc_test_surface = tcase_create("Test Surface Operations");
  tcase_add_test(tc_test_surface, test_ews_test_surface_contains_point);
  tcase_add_test(tc_test_surface, test_ews_test_surfaces_overlap);
  tcase_add_test(tc_test_surface, test_ews_test_surface_move);
  tcase_add_test(tc_test_surface, test_ews_test_surface_resize);
  suite_add_tcase(s, tc_test_surface);

  return s;
}

int main(void) {
  int number_failed = 0;
  SRunner *sr;

  /* Run integration and test surface tests */
  sr = srunner_create(ews_integration_suite());
  srunner_add_suite(sr, ews_test_surface_suite());

  srunner_run_all(sr, CK_NORMAL);
  number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);

  return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
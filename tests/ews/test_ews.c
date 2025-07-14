/* test_ews.c - Unit tests for ews mathematical and logical functions using
 * Check framework
 *
 * This file contains unit tests for functions that can be tested
 * without complex Wayland mocking.
 */

#include <check.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* ============================================================================
 * FUNCTIONS UNDER TEST
 * ============================================================================
 */

/* Pure function for surface bounds validation */
bool validate_surface_bounds(uint32_t x, uint32_t y, uint32_t width,
                             uint32_t height) {
  const uint32_t MAX_COORDINATE = 32768;
  const uint32_t MAX_SURFACE_SIZE = 16384;

  if (width == 0 || height == 0) {
    return false;
  }

  if (x >= MAX_COORDINATE || y >= MAX_COORDINATE) {
    return false;
  }

  if (width > MAX_SURFACE_SIZE || height > MAX_SURFACE_SIZE) {
    return false;
  }

  /* Check for overflow in coordinate calculations */
  if (x > MAX_COORDINATE - width || y > MAX_COORDINATE - height) {
    return false;
  }

  return true;
}

/* Simple coordinate normalization function */
void normalize_coordinates(int *x, int *y, int width, int height) {
  if (x && *x < 0) {
    *x = 0;
  }
  if (y && *y < 0) {
    *y = 0;
  }

  if (x && width > 0 && *x > width) {
    *x = width;
  }
  if (y && height > 0 && *y > height) {
    *y = height;
  }
}

/* Helper function to check if surface dimensions are valid */
bool is_valid_surface_size(uint32_t width, uint32_t height) {
  const uint32_t MIN_SIZE = 1;
  const uint32_t MAX_SIZE = 8192;

  return width >= MIN_SIZE && height >= MIN_SIZE && width <= MAX_SIZE &&
         height <= MAX_SIZE;
}

/* Calculate surface area (useful for memory allocation decisions) */
uint64_t calculate_surface_area(uint32_t width, uint32_t height) {
  return (uint64_t)width * (uint64_t)height;
}

/* Check if point is within surface bounds */
bool point_in_surface(int px, int py, int sx, int sy, uint32_t width,
                      uint32_t height) {
  return px >= sx && py >= sy && px < (sx + (int)width) &&
         py < (sy + (int)height);
}

/* Simple configuration parsing helper */
struct ews_config {
  char *startup_cmd;
  bool show_help;
  bool valid;
};

struct ews_config parse_single_arg(const char *arg, const char *value) {
  struct ews_config config = {0};

  if (!arg) {
    config.valid = false;
    return config;
  }

  if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) {
    config.show_help = true;
    config.valid = true;
  } else if (strcmp(arg, "-s") == 0) {
    if (value) {
      config.startup_cmd = strdup(value);
      config.valid = true;
    } else {
      config.valid = false;
    }
  } else {
    config.valid = false;
  }

  return config;
}

/* ============================================================================
 * TEST CASES
 * ============================================================================
 */

/* Test suite for validate_surface_bounds */
START_TEST(test_validate_surface_bounds_valid) {
  ck_assert(validate_surface_bounds(0, 0, 100, 100));
  ck_assert(validate_surface_bounds(1000, 1000, 800, 600));
  ck_assert(validate_surface_bounds(32767, 32767, 1, 1));
}
END_TEST

START_TEST(test_validate_surface_bounds_invalid) {
  ck_assert(!validate_surface_bounds(0, 0, 0, 100));
  ck_assert(!validate_surface_bounds(0, 0, 100, 0));
  ck_assert(!validate_surface_bounds(32768, 0, 100, 100));
  ck_assert(!validate_surface_bounds(0, 32768, 100, 100));
  ck_assert(!validate_surface_bounds(0, 0, 16385, 100));
  ck_assert(!validate_surface_bounds(0, 0, 100, 16385));
}
END_TEST

START_TEST(test_validate_surface_bounds_overflow) {
  ck_assert(!validate_surface_bounds(32767, 0, 2, 1));
  ck_assert(!validate_surface_bounds(0, 32767, 1, 2));
}
END_TEST

/* Test suite for normalize_coordinates */
START_TEST(test_normalize_coordinates_basic) {
  int x = -10, y = -20;
  normalize_coordinates(&x, &y, 1000, 800);
  ck_assert_int_eq(x, 0);
  ck_assert_int_eq(y, 0);

  x = 1500;
  y = 1000;
  normalize_coordinates(&x, &y, 1000, 800);
  ck_assert_int_eq(x, 1000);
  ck_assert_int_eq(y, 800);

  x = 500;
  y = 400;
  normalize_coordinates(&x, &y, 1000, 800);
  ck_assert_int_eq(x, 500);
  ck_assert_int_eq(y, 400);
}
END_TEST

START_TEST(test_normalize_coordinates_null_params) {
  int x = -10, y = -20;
  normalize_coordinates(NULL, &y, 1000, 800);
  ck_assert_int_eq(y, 0);

  x = -10;
  y = -20;
  normalize_coordinates(&x, NULL, 1000, 800);
  ck_assert_int_eq(x, 0);
}
END_TEST

/* Test suite for is_valid_surface_size */
START_TEST(test_is_valid_surface_size_valid) {
  ck_assert(is_valid_surface_size(100, 100));
  ck_assert(is_valid_surface_size(1, 1));
  ck_assert(is_valid_surface_size(8192, 8192));
}
END_TEST

START_TEST(test_is_valid_surface_size_invalid) {
  ck_assert(!is_valid_surface_size(0, 100));
  ck_assert(!is_valid_surface_size(100, 0));
  ck_assert(!is_valid_surface_size(8193, 100));
  ck_assert(!is_valid_surface_size(100, 8193));
}
END_TEST

/* Test suite for calculate_surface_area */
START_TEST(test_calculate_surface_area_normal) {
  ck_assert_uint_eq(calculate_surface_area(100, 100), 10000);
  ck_assert_uint_eq(calculate_surface_area(1920, 1080), 2073600);
  ck_assert_uint_eq(calculate_surface_area(1, 1), 1);
  ck_assert_uint_eq(calculate_surface_area(0, 100), 0);
}
END_TEST

START_TEST(test_calculate_surface_area_large) {
  /* Test for overflow prevention */
  ck_assert_uint_eq(calculate_surface_area(65536, 65536), 4294967296ULL);
}
END_TEST

/* Test suite for point_in_surface */
START_TEST(test_point_in_surface_inside) {
  ck_assert(point_in_surface(50, 50, 0, 0, 100, 100));
  ck_assert(point_in_surface(150, 150, 100, 100, 100, 100));
}
END_TEST

START_TEST(test_point_in_surface_boundary) {
  ck_assert(point_in_surface(0, 0, 0, 0, 100, 100));
  ck_assert(!point_in_surface(100, 100, 0, 0, 100, 100));
}
END_TEST

START_TEST(test_point_in_surface_outside) {
  ck_assert(!point_in_surface(-1, 50, 0, 0, 100, 100));
  ck_assert(!point_in_surface(101, 50, 0, 0, 100, 100));
  ck_assert(!point_in_surface(50, -1, 0, 0, 100, 100));
  ck_assert(!point_in_surface(50, 101, 0, 0, 100, 100));
  ck_assert(!point_in_surface(50, 50, 100, 100, 100, 100));
}
END_TEST

/* Test suite for parse_single_arg */
START_TEST(test_parse_single_arg_help) {
  struct ews_config config;

  config = parse_single_arg("-h", NULL);
  ck_assert(config.valid);
  ck_assert(config.show_help);

  config = parse_single_arg("--help", NULL);
  ck_assert(config.valid);
  ck_assert(config.show_help);
}
END_TEST

START_TEST(test_parse_single_arg_startup) {
  struct ews_config config;

  config = parse_single_arg("-s", "test_command");
  ck_assert(config.valid);
  ck_assert_str_eq(config.startup_cmd, "test_command");
  free(config.startup_cmd);

  config = parse_single_arg("-s", NULL);
  ck_assert(!config.valid);
}
END_TEST

START_TEST(test_parse_single_arg_invalid) {
  struct ews_config config;

  config = parse_single_arg("-x", NULL);
  ck_assert(!config.valid);

  config = parse_single_arg(NULL, NULL);
  ck_assert(!config.valid);
}
END_TEST

/* ============================================================================
 * TEST SUITE SETUP
 * ============================================================================
 */

static Suite *ews_suite(void) {
  Suite *s;
  TCase *tc_bounds, *tc_coords, *tc_size, *tc_area, *tc_point, *tc_args;

  s = suite_create("EWS Math/Logic Functions");

  /* Surface bounds validation tests */
  tc_bounds = tcase_create("Surface Bounds");
  tcase_add_test(tc_bounds, test_validate_surface_bounds_valid);
  tcase_add_test(tc_bounds, test_validate_surface_bounds_invalid);
  tcase_add_test(tc_bounds, test_validate_surface_bounds_overflow);
  suite_add_tcase(s, tc_bounds);

  /* Coordinate normalization tests */
  tc_coords = tcase_create("Coordinate Normalization");
  tcase_add_test(tc_coords, test_normalize_coordinates_basic);
  tcase_add_test(tc_coords, test_normalize_coordinates_null_params);
  suite_add_tcase(s, tc_coords);

  /* Surface size validation tests */
  tc_size = tcase_create("Surface Size Validation");
  tcase_add_test(tc_size, test_is_valid_surface_size_valid);
  tcase_add_test(tc_size, test_is_valid_surface_size_invalid);
  suite_add_tcase(s, tc_size);

  /* Surface area calculation tests */
  tc_area = tcase_create("Surface Area Calculation");
  tcase_add_test(tc_area, test_calculate_surface_area_normal);
  tcase_add_test(tc_area, test_calculate_surface_area_large);
  suite_add_tcase(s, tc_area);

  /* Point in surface tests */
  tc_point = tcase_create("Point in Surface");
  tcase_add_test(tc_point, test_point_in_surface_inside);
  tcase_add_test(tc_point, test_point_in_surface_boundary);
  tcase_add_test(tc_point, test_point_in_surface_outside);
  suite_add_tcase(s, tc_point);

  /* Argument parsing tests */
  tc_args = tcase_create("Argument Parsing");
  tcase_add_test(tc_args, test_parse_single_arg_help);
  tcase_add_test(tc_args, test_parse_single_arg_startup);
  tcase_add_test(tc_args, test_parse_single_arg_invalid);
  suite_add_tcase(s, tc_args);

  return s;
}

int main(void) {
  int number_failed;
  Suite *s;
  SRunner *sr;

  s = ews_suite();
  sr = srunner_create(s);

  srunner_run_all(sr, CK_NORMAL);
  number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);

  return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

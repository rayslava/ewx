/* ews_internal.h - Internal functions for EWS that can be unit tested
 *
 * This header exposes internal EWS functions for unit testing purposes.
 * These functions should not be used by external code.
 */

#ifndef EWS_INTERNAL_H
#define EWS_INTERNAL_H

#include <stdbool.h>
#include <stdint.h>

/* ============================================================================
 * ACTUAL EWS FUNCTIONS FOR TESTING
 * ============================================================================
 */

/* Configuration parsing struct - needed for both production and testing */
struct ews_config {
  char *startup_cmd;
  bool show_help;
  bool show_version;
  bool debug_mode;
  bool valid;
};

/* Forward declarations for structures and functions in ews.c */
#ifdef EWS_TESTING

/* Need Wayland types for function signatures */
#include <wayland-server.h>

/* Forward declarations for wlroots types */
struct wlr_xdg_surface;
struct wlr_xdg_toplevel;
struct wlr_scene_tree;
struct wlr_surface;

/* Forward declaration - actual struct defined in ews.c */
struct ews_surface;

/* Functions from ews.c that can be tested safely */
void focus_surface(struct ews_surface *ews_surface,
                   const struct wlr_surface *surface);

/* Forward declaration for server struct */
struct ews_server;

/* Surface lookup function with defensive programming patterns */
struct ews_surface *surface_at(struct ews_server *server, double lx, double ly,
                               struct wlr_surface **surface, double *sx,
                               double *sy);

#endif /* EWS_TESTING */

/* ============================================================================
 * FORWARD DECLARATIONS FOR TESTING
 * ============================================================================
 */

/* Forward declaration - actual struct defined in ews.c */
struct ews_surface;

/* ============================================================================
 * ACTUAL EWS FUNCTIONS EXPOSED FOR TESTING
 * ============================================================================
 */

/* Forward declarations for protocol functions - made testable via EWS_TESTING
 */
#ifdef EWS_TESTING

/* Need Wayland types for function signatures */
#include <wayland-server.h>

/* From ews.c - protocol handlers that we can test */
void ewp_surface_handle_layout(struct wl_client *client,
                               struct wl_resource *resource, uint32_t x,
                               uint32_t y, uint32_t width, uint32_t height);

void ewp_surface_handle_hide(struct wl_client *client,
                             struct wl_resource *resource);

void ewp_surface_handle_focus(struct wl_client *client,
                              struct wl_resource *resource);

#endif /* EWS_TESTING */

#endif /* EWS_INTERNAL_H */
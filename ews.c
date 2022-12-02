#define _POSIX_C_SOURCE 200112L
#include <assert.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_server_decoration.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>
#include "ewp-protocol.h"

/* For brevity's sake, struct members are annotated where they are used. */

struct ews_server {
  struct wl_display *wl_display;
  struct wlr_backend *backend;
  struct wlr_renderer *renderer;
  struct wlr_allocator *allocator;
  struct wlr_scene *scene;

  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wl_list surfaces;

  struct wlr_cursor *cursor;
  struct wlr_xcursor_manager *cursor_mgr;
  struct wl_listener cursor_motion;
  struct wl_listener cursor_motion_absolute;
  struct wl_listener cursor_button;
  struct wl_listener cursor_axis;
  struct wl_listener cursor_frame;

  struct wlr_seat *seat;
  struct wl_listener new_input;
  struct wl_listener request_cursor;
  struct wl_listener request_set_selection;
  struct wl_list keyboards;

  struct wlr_output_layout *output_layout;
  struct wl_list outputs;
  struct wl_listener new_output;

  struct wl_resource *layout_resource;
};

struct ews_output {
  struct wl_list link;
  struct ews_server *server;
  struct wlr_output *wlr_output;
  struct wl_listener frame;
  struct wl_listener destroy;
};

/* A ews_surface is a ~ xdg_toplevel.
   Maybe other name? But name is in protocol and elisp too already.
   Keep. Is a fitting name.*/
struct ews_surface {
  struct wl_list link;
  struct ews_server *server;
  struct wlr_xdg_surface *xdg_surface;
  struct wlr_xdg_toplevel *xdg_toplevel;
  struct wlr_scene_tree *scene_tree;
  struct wl_resource *ewp_surface;
  int x, y, width, height;
  bool mapped;
  struct wl_listener map;
  struct wl_listener unmap;
  struct wl_listener destroy;
  struct wl_listener request_maximize;
  struct wl_listener request_fullscreen;
};

struct ews_keyboard {
  struct wl_list link;
  struct ews_server *server;
  struct wlr_keyboard *wlr_keyboard;

  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
};

static void focus_surface(struct ews_surface *ews_surface, struct wlr_surface *surface) {
  /* Note: this function only deals with keyboard focus. */
  if (ews_surface == NULL) {
    return;
  }
  struct ews_server *server = ews_surface->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    /* Don't re-focus an already focused surface. */
    return;
  }
  if (prev_surface) {
    /*
     * Deactivate the previously focused surface. This lets the client know
     * it no longer has focus and the client will repaint accordingly, e.g.
     * stop displaying a caret.
     */
    struct wlr_xdg_surface *previous = wlr_xdg_surface_from_wlr_surface(seat->keyboard_state.focused_surface);
    assert(previous->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);
    wlr_xdg_toplevel_set_activated(previous->toplevel, false);
  }
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  /* Activate the new surface */
  wlr_xdg_toplevel_set_activated(ews_surface->xdg_toplevel, true);
  /*
   * Tell the seat to have the keyboard enter this surface. wlroots will keep
   * track of this and automatically send key events to the appropriate
   * clients without additional work on your part.
   */
  if (keyboard != NULL) {
    wlr_seat_keyboard_notify_enter(seat, ews_surface->xdg_toplevel->base->surface,
                                   keyboard->keycodes, keyboard->num_keycodes, &keyboard->modifiers);
  }

  /* Issue a focus event */
  ewp_surface_send_focus(ews_surface->ewp_surface);
}

static void keyboard_handle_modifiers(
                                      struct wl_listener *listener, void *data) {
  /* This event is raised when a modifier key, such as shift or alt, is
   * pressed. We simply communicate this to the client. */
  struct ews_keyboard *keyboard =
    wl_container_of(listener, keyboard, modifiers);
  /*
   * A seat can only have one keyboard, but this is a limitation of the
   * Wayland protocol - not wlroots. We assign all connected keyboards to the
   * same seat. You can swap out the underlying wlr_keyboard like this and
   * wlr_seat handles this transparently.
   */
  wlr_seat_set_keyboard(keyboard->server->seat, keyboard->wlr_keyboard);
  /* Send modifiers to the client. */
  wlr_seat_keyboard_notify_modifiers(keyboard->server->seat,
                                     &keyboard->wlr_keyboard->modifiers);
}

static void keyboard_handle_key(
                                struct wl_listener *listener, void *data) {
  /* This event is raised when a key is pressed or released. */
  struct ews_keyboard *keyboard =
    wl_container_of(listener, keyboard, key);
  struct ews_server *server = keyboard->server;
  struct wlr_keyboard_key_event *event = data;
  struct wlr_seat *seat = server->seat;

  /* Pass it along to the client. */
  wlr_seat_set_keyboard(seat, keyboard->wlr_keyboard);
  wlr_seat_keyboard_notify_key(seat, event->time_msec,
                               event->keycode, event->state);
}

static void keyboard_handle_destroy(struct wl_listener *listener, void *data) {
  /* This event is raised by the keyboard base wlr_input_device to signal
   * the destruction of the wlr_keyboard. It will no longer receive events
   * and should be destroyed.
   */
  struct ews_keyboard *keyboard =
    wl_container_of(listener, keyboard, destroy);
  wl_list_remove(&keyboard->modifiers.link);
  wl_list_remove(&keyboard->key.link);
  wl_list_remove(&keyboard->destroy.link);
  wl_list_remove(&keyboard->link);
  free(keyboard);
}

static void server_new_keyboard(struct ews_server *server,
                                struct wlr_input_device *device) {
  struct wlr_keyboard *wlr_keyboard = wlr_keyboard_from_input_device(device);

  struct ews_keyboard *keyboard =
    calloc(1, sizeof(struct ews_keyboard));
  keyboard->server = server;
  keyboard->wlr_keyboard = wlr_keyboard;

  /* We need to prepare an XKB keymap and assign it to the keyboard. This
   * assumes the defaults (e.g. layout = "us"). */
  struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap = xkb_keymap_new_from_names(context, NULL,
                                                        XKB_KEYMAP_COMPILE_NO_FLAGS);

  wlr_keyboard_set_keymap(wlr_keyboard, keymap);
  xkb_keymap_unref(keymap);
  xkb_context_unref(context);
  wlr_keyboard_set_repeat_info(wlr_keyboard, 25, 600);

  /* Here we set up listeners for keyboard events. */
  keyboard->modifiers.notify = keyboard_handle_modifiers;
  wl_signal_add(&wlr_keyboard->events.modifiers, &keyboard->modifiers);
  keyboard->key.notify = keyboard_handle_key;
  wl_signal_add(&wlr_keyboard->events.key, &keyboard->key);
  keyboard->destroy.notify = keyboard_handle_destroy;
  wl_signal_add(&device->events.destroy, &keyboard->destroy);

  wlr_seat_set_keyboard(server->seat, keyboard->wlr_keyboard);

  /* And add the keyboard to our list of keyboards */
  wl_list_insert(&server->keyboards, &keyboard->link);
}

static void server_new_pointer(struct ews_server *server,
                               struct wlr_input_device *device) {
  /* We don't do anything special with pointers. All of our pointer handling
   * is proxied through wlr_cursor. On another compositor, you might take this
   * opportunity to do libinput configuration on the device to set
   * acceleration, etc. */
  wlr_cursor_attach_input_device(server->cursor, device);
}

static void server_new_input(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new input device becomes
   * available. */
  struct ews_server *server =
    wl_container_of(listener, server, new_input);
  struct wlr_input_device *device = data;
  switch (device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    server_new_keyboard(server, device);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    server_new_pointer(server, device);
    break;
  default:
    break;
  }
  /* We need to let the wlr_seat know what our capabilities are, which is
   * communiciated to the client. In Ews we always have a cursor, even if
   * there are no pointer devices, so we always include that capability. */
  uint32_t caps = WL_SEAT_CAPABILITY_POINTER;
  if (!wl_list_empty(&server->keyboards)) {
    caps |= WL_SEAT_CAPABILITY_KEYBOARD;
  }
  wlr_seat_set_capabilities(server->seat, caps);
}

static void seat_request_cursor(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(
                                                 listener, server, request_cursor);
  /* This event is raised by the seat when a client provides a cursor image */
  struct wlr_seat_pointer_request_set_cursor_event *event = data;
  struct wlr_seat_client *focused_client =
    server->seat->pointer_state.focused_client;
  /* This can be sent by any client, so we check to make sure this one is
   * actually has pointer focus first. */
  if (focused_client == event->seat_client) {
    /* Once we've vetted the client, we can tell the cursor to use the
     * provided surface as the cursor image. It will set the hardware cursor
     * on the output that it's currently on and continue to do so as the
     * cursor moves between outputs. */
    wlr_cursor_set_surface(server->cursor, event->surface,
                           event->hotspot_x, event->hotspot_y);
  }
}

static void seat_request_set_selection(struct wl_listener *listener, void *data) {
  /* This event is raised by the seat when a client wants to set the selection,
   * usually when the user copies something. wlroots allows compositors to
   * ignore such requests if they so choose, but in ews we always honor
   */
  struct ews_server *server = wl_container_of(
                                                 listener, server, request_set_selection);
  struct wlr_seat_request_set_selection_event *event = data;
  wlr_seat_set_selection(server->seat, event->source, event->serial);
}

static struct ews_surface *surface_at(struct ews_server *server, double lx, double ly,
                                      struct wlr_surface **surface, double *sx, double *sy) {
  /* This returns the topmost node in the scene at the given layout coords.
   * we only care about surface nodes as we are specifically looking for a
   * surface in the surface tree of a ews_surface. */
  struct wlr_scene_node *node = wlr_scene_node_at(&server->scene->tree.node,
                                                  lx, ly, sx, sy);
  if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
    return NULL;
  }
  struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
  struct wlr_scene_surface *scene_surface =
    wlr_scene_surface_from_buffer(scene_buffer);
  if (!scene_surface) {
    return NULL;
  }

  *surface = scene_surface->surface;
  /* Find the node corresponding to the ews_surface at the root of this
   * surface tree, it is the only one for which we set the data field. */
  struct wlr_scene_tree *tree = node->parent;
  while (tree != NULL && tree->node.data == NULL) {
    tree = tree->node.parent;
  }
  return tree->node.data;
}

static void process_cursor_motion(struct ews_server *server, uint32_t time) {
  /* Find the view under the pointer and send the event along. */
  double sx, sy;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *surface = NULL;
  struct ews_surface *ews_surface = surface_at(server,
                                               server->cursor->x, server->cursor->y,
                                               &surface, &sx, &sy);
  if (!surface) {
    /* If there's no view under the cursor, set the cursor image to a
     * default. This is what makes the cursor image appear when you move it
     * around the screen, not over any views. */
    wlr_xcursor_manager_set_cursor_image(server->cursor_mgr, "left_ptr", server->cursor);
  }
  if (surface) {
    /*
     * Send pointer enter and motion events.
     *
     * The enter event gives the surface "pointer focus", which is distinct
     * from keyboard focus. You get pointer focus by moving the pointer over
     * a window.
     *
     * Note that wlroots will avoid sending duplicate enter/motion events if
     * the surface has already has pointer focus or if the client is already
     * aware of the coordinates passed.
     */
    wlr_seat_pointer_notify_enter(seat, surface, sx, sy);
    wlr_seat_pointer_notify_motion(seat, time, sx, sy);
  } else {
    /* Clear pointer focus so future button events and such are not sent to
     * the last client to have the cursor over it. */
    wlr_seat_pointer_clear_focus(seat);
  }
}

static void server_cursor_motion(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits a _relative_
   * pointer motion event (i.e. a delta) */
  struct ews_server *server =
    wl_container_of(listener, server, cursor_motion);
  struct wlr_pointer_motion_event *event = data;
  /* The cursor doesn't move unless we tell it to. The cursor automatically
   * handles constraining the motion to the output layout, as well as any
   * special configuration applied for the specific input device which
   * generated the event. You can pass NULL for the device if you want to move
   * the cursor around without any input. */
  wlr_cursor_move(server->cursor, &event->pointer->base,
                  event->delta_x, event->delta_y);
  process_cursor_motion(server, event->time_msec);
}

static void server_cursor_motion_absolute(
                                          struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits an _absolute_
   * motion event, from 0..1 on each axis. This happens, for example, when
   * wlroots is running under a Wayland window rather than KMS+DRM, and you
   * move the mouse over the window. You could enter the window from any edge,
   * so we have to warp the mouse there. There is also some hardware which
   * emits these events. */
  struct ews_server *server =
    wl_container_of(listener, server, cursor_motion_absolute);
  struct wlr_pointer_motion_absolute_event *event = data;
  wlr_cursor_warp_absolute(server->cursor, &event->pointer->base, event->x,
                           event->y);
  process_cursor_motion(server, event->time_msec);
}

static void server_cursor_button(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits a button
   * event. */
  struct ews_server *server =
    wl_container_of(listener, server, cursor_button);
  struct wlr_pointer_button_event *event = data;
  /* Notify the client with pointer focus that a button press has occurred */
  wlr_seat_pointer_notify_button(server->seat,
                                 event->time_msec, event->button, event->state);
  double sx, sy;
  struct wlr_surface *surface = NULL;
  struct ews_surface *ews_surface = surface_at(server,
                                              server->cursor->x, server->cursor->y,
                                               &surface, &sx, &sy);
  if (event->state == WLR_BUTTON_PRESSED) {
    /* Focus client where button was _pressed_ */
    focus_surface(ews_surface, surface);
  }
}

static void server_cursor_axis(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits an axis event,
   * for example when you move the scroll wheel. */
  struct ews_server *server =
    wl_container_of(listener, server, cursor_axis);
  struct wlr_pointer_axis_event *event = data;
  /* Notify the client with pointer focus of the axis event. */
  wlr_seat_pointer_notify_axis(server->seat,
                               event->time_msec, event->orientation, event->delta,
                               event->delta_discrete, event->source);
}

static void server_cursor_frame(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits an frame
   * event. Frame events are sent after regular pointer events to group
   * multiple events together. For instance, two axis events may happen at the
   * same time, in which case a frame event won't be sent in between. */
  struct ews_server *server =
    wl_container_of(listener, server, cursor_frame);
  /* Notify the client with pointer focus of the frame event. */
  wlr_seat_pointer_notify_frame(server->seat);
}

static void output_frame(struct wl_listener *listener, void *data) {
  /* This function is called every time an output is ready to display a frame,
   * generally at the output's refresh rate (e.g. 60Hz). */
  struct ews_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->server->scene;

  struct wlr_scene_output *scene_output = wlr_scene_get_scene_output(scene, output->wlr_output);

  /* Render the scene if needed and commit the output */
  wlr_scene_output_commit(scene_output);

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(scene_output, &now);
}

static void output_destroy(struct wl_listener *listener, void *data) {
  struct ews_output *output = wl_container_of(listener, output, destroy);

  wl_list_remove(&output->frame.link);
  wl_list_remove(&output->destroy.link);
  wl_list_remove(&output->link);
  free(output);
}

static void server_new_output(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new output (aka a display or
   * monitor) becomes available. */
  struct ews_server *server =
    wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  /* Configures the output created by the backend to use our allocator
   * and our renderer. Must be done once, before commiting the output */
  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  /* Some backends don't have modes. DRM+KMS does, and we need to set a mode
   * before we can use the output. The mode is a tuple of (width, height,
   * refresh rate), and each monitor supports only a specific set of modes. We
   * just pick the monitor's preferred mode, a more sophisticated compositor
   * would let the user configure it. */
  if (!wl_list_empty(&wlr_output->modes)) {
    struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    wlr_output_set_mode(wlr_output, mode);
    wlr_output_enable(wlr_output, true);
    if (!wlr_output_commit(wlr_output)) {
      return;
    }
  }

  /* Allocates and configures our state for this output */
  struct ews_output *output =
    calloc(1, sizeof(struct ews_output));
  output->wlr_output = wlr_output;
  output->server = server;
  /* Sets up a listener for the frame notify event. */
  output->frame.notify = output_frame;
  wl_signal_add(&wlr_output->events.frame, &output->frame);

  /* Sets up a listener for the destroy notify event. */
  output->destroy.notify = output_destroy;
  wl_signal_add(&wlr_output->events.destroy, &output->destroy);

  wl_list_insert(&server->outputs, &output->link);

  /* Adds this to the output layout. The add_auto function arranges outputs
   * from left-to-right in the order they appear. A more sophisticated
   * compositor would let the user configure the arrangement of outputs in the
   * layout.
   *
   * The output layout utility automatically adds a wl_output global to the
   * display, which Wayland clients can see to find out information about the
   * output (such as DPI, scale factor, manufacturer, etc).
   */
  wlr_output_layout_add_auto(server->output_layout, wlr_output);
}

static void layout_surface(struct ews_surface *surface) {
  /* Add scene_tree for surface if missing */
  if (surface->scene_tree == NULL) {
    surface->scene_tree = wlr_scene_xdg_surface_create(&surface->server->scene->tree,
                                                       surface->xdg_toplevel->base);
    /* For surface_at */
    surface->scene_tree->node.data = surface;
    /* For popup */
    surface->xdg_surface->data = surface->scene_tree;
  } else {
    wlr_scene_node_set_enabled(&surface->scene_tree->node, true);
  }

  wlr_xdg_toplevel_set_size(surface->xdg_toplevel, surface->width, surface->height);
  wlr_scene_node_set_position(&surface->scene_tree->node, surface->x, surface->y);
}

static void xdg_toplevel_map(struct wl_listener *listener, void *data) {
  /* Called when the surface is mapped, or ready to display on-screen. */
  struct ews_surface *surface = wl_container_of(listener, surface, map);
  
  surface->mapped = true;
  
  if (surface->width != 0
      && surface->height != 0) {
    layout_surface(surface);
  }
}

static void xdg_toplevel_unmap(struct wl_listener *listener, void *data) {
  /* Called when the surface is unmapped, and should no longer be shown. */
  struct ews_surface *surface = wl_container_of(listener, surface, unmap);

  /* Not handled, because emacs decides when to unmap. */
  wlr_log(WLR_ERROR, "Attention: Someone called xdg_toplevel_unmap");
}

static void ewp_surface_destroy(struct wl_resource *resource) {
  ewp_surface_send_destroy(resource);
  struct ews_surface *surface = wl_resource_get_user_data(resource);

  wl_list_remove(&surface->link);
  wl_list_remove(&surface->map.link);
  wl_list_remove(&surface->unmap.link);
  wl_list_remove(&surface->destroy.link);
  wl_list_remove(&surface->request_maximize.link);
  wl_list_remove(&surface->request_fullscreen.link);

  free(surface);
}

static void xdg_toplevel_destroy(struct wl_listener *listener, void *data) {
  /* Called when the surface is destroyed and should never be shown again. */
  struct ews_surface *surface = wl_container_of(listener, surface, destroy);
  wl_resource_destroy(surface->ewp_surface);
}

static void xdg_toplevel_request_maximize(struct wl_listener *listener, void *data) {
  /* This event is raised when a client would like to maximize itself,
   * typically because the user clicked on the maximize button on
   * client-side decorations. ews doesn't support maximization, but
   * to conform to xdg-shell protocol we still must send a configure.
   * wlr_xdg_surface_schedule_configure() is used to send an empty reply. */
  struct ews_surface *surface =
    wl_container_of(listener, surface, request_maximize);
  wlr_xdg_surface_schedule_configure(surface->xdg_toplevel->base);
}

static void xdg_toplevel_request_fullscreen(struct wl_listener *listener, void *data) {
  /* Just as with request_maximize, we must send a configure here. */
  struct ews_surface *surface =
    wl_container_of(listener, surface, request_fullscreen);
  wlr_xdg_surface_schedule_configure(surface->xdg_toplevel->base);
}



static void ewp_surface_handle_layout(struct wl_client *client, struct wl_resource *resource,
                                      uint32_t x, uint32_t y,
                                      uint32_t width, uint32_t height) {
  wlr_log(WLR_DEBUG, "Laying out surface x=%d y=%d width=%d height=%d", x, y, width, height);

  struct ews_surface *surface = wl_resource_get_user_data(resource);

  surface->x = x;
  surface->y = y;
  surface->width = width;
  surface->height = height;
  if (surface->mapped) {
      layout_surface(surface);
  }
}

static void ewp_surface_handle_hide(struct wl_client *client, struct wl_resource *resource) {
  struct ews_surface *surface = wl_resource_get_user_data(resource);
  if (surface->scene_tree != NULL) {
    wlr_scene_node_set_enabled(&surface->scene_tree->node, false);
  }
}

static void ewp_surface_handle_client_destroy(struct wl_client *client, struct wl_resource *resource) {
  wl_resource_destroy(resource);
}

static const struct ewp_surface_interface
ewp_surface_implementation = {
  .layout = ewp_surface_handle_layout,
  .hide = ewp_surface_handle_hide,
  .destroy = ewp_surface_handle_client_destroy,
};
/* TODO: 2 events destroy & update_title */

static void server_new_xdg_surface(struct wl_listener *listener, void *data) {
  /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
   * client, either a toplevel (application window) or popup. */

  /** From wlr_xdg_shell.h
   * The `new_surface` event signals that a client has requested to
   * create a new shell surface. At this point, the surface is ready to
   * be configured but is not mapped or ready receive input events. The
   * surface will be ready to be managed on the `map` event.
   */

  struct ews_server *server =
    wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  /* DEBUG ignore for extra ews */
  if (server->layout_resource == NULL) {
    return;
  }

  /* We must add xdg popups to the scene graph so they get rendered. The
   * wlroots scene graph provides a helper for this, but to use it we must
   * provide the proper parent scene node of the xdg popup. To enable this,
   * we always set the user data field of xdg_surfaces to the corresponding
   * scene node. */
  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent = wlr_xdg_surface_from_wlr_surface(xdg_surface->popup->parent);
    struct wlr_scene_tree *parent_tree = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_tree, xdg_surface);
    return;
  }

  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);

  /* Allocate a ews_surface for this surface */
  struct ews_surface *surface =
    calloc(1, sizeof(struct ews_surface));
  wl_list_insert(&server->surfaces, &surface->link);
  surface->server = server;
  surface->xdg_surface = xdg_surface;
  surface->xdg_toplevel = xdg_surface->toplevel;

  /* Create an ewp_surface */
  struct wl_client *client = wl_resource_get_client(server->layout_resource);
  struct wl_resource *resource = wl_resource_create(client, &ewp_surface_interface, 1, 0);
  wl_resource_set_implementation(resource, &ewp_surface_implementation,
                                 surface, ewp_surface_destroy);
  surface->ewp_surface = resource;

  pid_t pid;
  wl_client_get_credentials(xdg_surface->client->client, &pid, 0, 0);
  wlr_log(WLR_DEBUG, "New toplevel XDG surface app_id=%s title=%s pid=%d",
          xdg_surface->toplevel->app_id, xdg_surface->toplevel->title, pid);
  ewp_layout_send_new_surface(server->layout_resource,
                              resource, 
                              xdg_surface->toplevel->app_id,
                              xdg_surface->toplevel->title,
                              pid);

  /* Listen to the various events it can emit */
  surface->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->events.map, &surface->map);
  surface->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->events.unmap, &surface->unmap);
  surface->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &surface->destroy);

  /* cotd */
  struct wlr_xdg_toplevel *toplevel = xdg_surface->toplevel;
  surface->request_maximize.notify = xdg_toplevel_request_maximize;
  wl_signal_add(&toplevel->events.request_maximize,
                &surface->request_maximize);
  surface->request_fullscreen.notify = xdg_toplevel_request_fullscreen;
  wl_signal_add(&toplevel->events.request_fullscreen,
                &surface->request_fullscreen);
}

struct ewp_layout { 
  struct ews_server *server;
};

static void ewp_layout_handle_bind(struct wl_client *client, void *data,
                                   uint32_t version, uint32_t id) {
  struct ewp_layout *layout = data;

  if (layout->server->layout_resource != NULL) {
    wlr_log(WLR_ERROR, "Error: 2nd layout client tried to connect but only one allowed");
    return;
  }

  struct wl_resource *resource = wl_resource_create(client, &ewp_layout_interface,
                                                    version, id);

  /* last=NULL is handle_resource_destroy TODO add maybe */
  /* wl_resource_set_implementation(resource, &ewp_layout_interface, layout, NULL); */

  layout->server->layout_resource = resource;
}

int main(int argc, char *argv[]) {
  wlr_log_init(WLR_DEBUG, NULL);
  char *startup_cmd = NULL;

  int c;
  while ((c = getopt(argc, argv, "s:h")) != -1) {
    switch (c) {
    case 's':
      startup_cmd = optarg;
      break;
    default:
      printf("Usage: %s [-s startup command]\n", argv[0]);
      return 0;
    }
  }
  if (optind < argc) {
    printf("Usage: %s [-s startup command]\n", argv[0]);
    return 0;
  }

  struct ews_server server;
  /* The Wayland display is managed by libwayland. It handles accepting
   * clients from the Unix socket, manging Wayland globals, and so on. */
  server.wl_display = wl_display_create();
  /* The backend is a wlroots feature which abstracts the underlying input and
   * output hardware. The autocreate option will choose the most suitable
   * backend based on the current environment, such as opening an X11 window
   * if an X11 server is running. */
  server.backend = wlr_backend_autocreate(server.wl_display);
  if (server.backend == NULL) {
    wlr_log(WLR_ERROR, "failed to create wlr_backend");
    return 1;
  }

  /* Autocreates a renderer, either Pixman, GLES2 or Vulkan for us. The user
   * can also specify a renderer using the WLR_RENDERER env var.
   * The renderer is responsible for defining the various pixel formats it
   * supports for shared memory, this configures that for clients. */
  server.renderer = wlr_renderer_autocreate(server.backend);
  if (server.renderer == NULL) {
    wlr_log(WLR_ERROR, "failed to create wlr_renderer");
    return 1;
  }

  wlr_renderer_init_wl_display(server.renderer, server.wl_display);

  /* Autocreates an allocator for us.
   * The allocator is the bridge between the renderer and the backend. It
   * handles the buffer creation, allowing wlroots to render onto the
   * screen */
  server.allocator = wlr_allocator_autocreate(server.backend,
                                              server.renderer);
  if (server.allocator == NULL) {
    wlr_log(WLR_ERROR, "failed to create wlr_allocator");
    return 1;
  }

  /* This creates some hands-off wlroots interfaces. The compositor is
   * necessary for clients to allocate surfaces, the subcompositor allows to
   * assign the role of subsurfaces to surfaces and the data device manager
   * handles the clipboard. Each of these wlroots interfaces has room for you
   * to dig your fingers in and play with their behavior if you want. Note that
   * the clients cannot set the selection directly without compositor approval,
   * see the handling of the request_set_selection event below.*/
  wlr_compositor_create(server.wl_display, server.renderer);
  wlr_subcompositor_create(server.wl_display);
  wlr_data_device_manager_create(server.wl_display);

  /* Creates an output layout, which a wlroots utility for working with an
   * arrangement of screens in a physical layout. */
  server.output_layout = wlr_output_layout_create();

  /* Configure a listener to be notified when new outputs are available on the
   * backend. */
  wl_list_init(&server.outputs);
  server.new_output.notify = server_new_output;
  wl_signal_add(&server.backend->events.new_output, &server.new_output);

  /* Create a scene graph. This is a wlroots abstraction that handles all
   * rendering and damage tracking. All the compositor author needs to do
   * is add things that should be rendered to the scene graph at the proper
   * positions and then call wlr_scene_output_commit() to render a frame if
   * necessary.
   */
  server.scene = wlr_scene_create();
  wlr_scene_attach_output_layout(server.scene, server.output_layout);

  /* Create xdg_output_manager. This provides reliable x, y, height
     and with per layout. */
  wlr_xdg_output_manager_v1_create(server.wl_display, server.output_layout);

  /* Set up xdg-shell version 3. The xdg-shell is a Wayland protocol which is
   * used for application windows. For more detail on shells, refer to my
   * article:
   *
   * https://drewdevault.com/2018/07/29/Wayland-shells.html
   */
  wl_list_init(&server.surfaces);
  server.xdg_shell = wlr_xdg_shell_create(server.wl_display, 3);
  server.new_xdg_surface.notify = server_new_xdg_surface;
  wl_signal_add(&server.xdg_shell->events.new_surface,
                &server.new_xdg_surface);

  /*
   * Create xdg_decoration_manager. This makes most applications
   * draw no decorations. This results in no decorations at all
   * because we don't add server side decorations.
   */
  wlr_xdg_decoration_manager_v1_create(server.wl_display);
  /*
   * Make it work with GTK too. GTK still uses the (now
   * deprecated) server-decoration protocol, which is the
   * predecessor to xdg-decoration.
   */
  wlr_server_decoration_manager_set_default_mode(
    wlr_server_decoration_manager_create(server.wl_display),
    WLR_SERVER_DECORATION_MANAGER_MODE_SERVER);

  /*
   * Creates a cursor, which is a wlroots utility for tracking the cursor
   * image shown on screen.
   */
  server.cursor = wlr_cursor_create();
  wlr_cursor_attach_output_layout(server.cursor, server.output_layout);

  /* Creates an xcursor manager, another wlroots utility which loads up
   * Xcursor themes to source cursor images from and makes sure that cursor
   * images are available at all scale factors on the screen (necessary for
   * HiDPI support). We add a cursor theme at scale factor 1 to begin with. */
  server.cursor_mgr = wlr_xcursor_manager_create(NULL, 24);
  wlr_xcursor_manager_load(server.cursor_mgr, 1);

  /*
   * wlr_cursor *only* displays an image on screen. It does not move around
   * when the pointer moves. However, we can attach input devices to it, and
   * it will generate aggregate events for all of them. In these events, we
   * can choose how we want to process them, forwarding them to clients and
   * moving the cursor around. More detail on this process is described in my
   * input handling blog post:
   *
   * https://drewdevault.com/2018/07/17/Input-handling-in-wlroots.html
   *
   * And more comments are sprinkled throughout the notify functions above.
   */
  server.cursor_motion.notify = server_cursor_motion;
  wl_signal_add(&server.cursor->events.motion, &server.cursor_motion);
  server.cursor_motion_absolute.notify = server_cursor_motion_absolute;
  wl_signal_add(&server.cursor->events.motion_absolute,
                &server.cursor_motion_absolute);
  server.cursor_button.notify = server_cursor_button;
  wl_signal_add(&server.cursor->events.button, &server.cursor_button);
  server.cursor_axis.notify = server_cursor_axis;
  wl_signal_add(&server.cursor->events.axis, &server.cursor_axis);
  server.cursor_frame.notify = server_cursor_frame;
  wl_signal_add(&server.cursor->events.frame, &server.cursor_frame);

  /*
   * Configures a seat, which is a single "seat" at which a user sits and
   * operates the computer. This conceptually includes up to one keyboard,
   * pointer, touch, and drawing tablet device. We also rig up a listener to
   * let us know when new input devices are available on the backend.
   */
  wl_list_init(&server.keyboards);
  server.new_input.notify = server_new_input;
  wl_signal_add(&server.backend->events.new_input, &server.new_input);
  server.seat = wlr_seat_create(server.wl_display, "seat0");
  server.request_cursor.notify = seat_request_cursor;
  wl_signal_add(&server.seat->events.request_set_cursor,
                &server.request_cursor);
  server.request_set_selection.notify = seat_request_set_selection;
  wl_signal_add(&server.seat->events.request_set_selection,
                &server.request_set_selection);

  /* ewp */
  struct ewp_layout layout;
  layout.server = &server;
  server.layout_resource = NULL;
  wlr_log(WLR_DEBUG, "server layout_resource %d", server.layout_resource);

  /* wl_display, wl_interface, version, *data, bind */
  wl_global_create(server.wl_display, &ewp_layout_interface,
                   1, &layout, ewp_layout_handle_bind);

  /* Add a Unix socket to the Wayland display. */
  const char *socket = wl_display_add_socket_auto(server.wl_display);
  if (!socket) {
    wlr_backend_destroy(server.backend);
    return 1;
  }

  /* Start the backend. This will enumerate outputs and inputs, become the DRM
   * master, etc */
  if (!wlr_backend_start(server.backend)) {
    wlr_backend_destroy(server.backend);
    wl_display_destroy(server.wl_display);
    return 1;
  }

  /* Set the WAYLAND_DISPLAY environment variable to our socket */
  setenv("WAYLAND_DISPLAY", socket, true);
        
  /* pid_t pid = fork(); */
  /* if (pid == 0) { */
  /*   execl("/gnu/store/aaga7qf0y93rfxrkwmqwh9z1fpcdn7ii-emacs-next-pgtk-29.0.50-1.0a5477b/bin/emacs", (void *)NULL); */
  /* } */
  /* server.emacs_pid = pid; */
        
  /* Run the Wayland event loop. This does not return until you exit the
   * compositor. Starting the backend rigged up all of the necessary event
   * loop configuration to listen to libinput events, DRM events, generate
   * frame events at the refresh rate, and so on. */
  wlr_log(WLR_INFO, "Running Wayland compositor on WAYLAND_DISPLAY=%s", socket);
  wl_display_run(server.wl_display);

  /* Once wl_display_run returns, we shut down the server. */
  wl_display_destroy_clients(server.wl_display);
  wl_display_destroy(server.wl_display);
  return 0;
}

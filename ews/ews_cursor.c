/* ews_cursor.c --- Emacs wayland server cursor and pointer management

   Copyright (C) 2023  Michael Bauer
                 2025  Slava Barinov

   Author: Michael Bauer <michael-bauer@posteo.de>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.

   Cursor movement, surface detection, and pointer event handling.
*/

#define _POSIX_C_SOURCE 200112L

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>

#include "ews_cursor.h"
#include "ews_internal.h"

struct ews_surface *surface_at(struct ews_server *server, double lx, double ly,
                               struct wlr_surface **surface, double *sx, double *sy) {
  struct ews_output *output;
  wl_list_for_each(output, &server->outputs, link) {
    if (!output->scene) {
      continue;
    }

    struct wlr_box output_box;
    wlr_output_layout_get_box(server->output_layout, output->wlr_output, &output_box);

    double local_x = lx - output_box.x;
    double local_y = ly - output_box.y;

    if (local_x < 0 || local_y < 0 || local_x >= output_box.width ||
        local_y >= output_box.height) {
      continue;
    }

    struct wlr_scene_node *node =
        wlr_scene_node_at(&output->scene->tree.node, local_x, local_y, sx, sy);
    if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
      continue;
    }

    struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
    const struct wlr_scene_surface *scene_surface =
        wlr_scene_surface_try_from_buffer(scene_buffer);
    if (!scene_surface) {
      continue;
    }

    *surface = scene_surface->surface;
    struct wlr_scene_tree *tree = node->parent;
    while (tree != NULL && tree->node.data == NULL) {
      tree = tree->node.parent;
    }
    if (tree) {
      return tree->node.data;
    }
  }

  return NULL;
}

static void process_cursor_motion(struct ews_server *server, uint32_t time) {
  double sx, sy;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *surface = NULL;
  struct ews_surface *ews_surface =
      surface_at(server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  wlr_log(WLR_DEBUG, "Cursor at (%.1f,%.1f) -> %s (sx=%.1f sy=%.1f)", server->cursor->x,
          server->cursor->y, surface ? "on-surface" : "no-surface", sx, sy);
  (void)ews_surface; /* Suppress unused warning for now */
  if (!surface) {
    wlr_cursor_set_xcursor(server->cursor, server->cursor_mgr, "left_ptr");
  }
  if (surface) {
    wlr_seat_pointer_notify_enter(seat, surface, sx, sy);
    wlr_seat_pointer_notify_motion(seat, time, sx, sy);
  } else {
    wlr_seat_pointer_clear_focus(seat);
  }
}

void server_cursor_motion(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, cursor_motion);
  struct wlr_pointer_motion_event *event = data;
  wlr_cursor_move(server->cursor, &event->pointer->base, event->delta_x, event->delta_y);
  process_cursor_motion(server, event->time_msec);
}

void server_cursor_motion_absolute(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, cursor_motion_absolute);
  struct wlr_pointer_motion_absolute_event *event = data;
  wlr_cursor_warp_absolute(server->cursor, &event->pointer->base, event->x, event->y);
  process_cursor_motion(server, event->time_msec);
}

void server_cursor_button(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, cursor_button);
  struct wlr_pointer_button_event *event = data;
  wlr_seat_pointer_notify_button(server->seat, event->time_msec, event->button, event->state);
  double sx, sy;
  struct wlr_surface *surface = NULL;
  struct ews_surface *ews_surface =
      surface_at(server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (event->state == WLR_BUTTON_PRESSED) {
    focus_surface(ews_surface, surface);
  }
}

void server_cursor_axis(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, cursor_axis);
  struct wlr_pointer_axis_event *event = data;
  wlr_seat_pointer_notify_axis(server->seat, event->time_msec, event->orientation, event->delta,
                               event->delta_discrete, event->source);
}

void server_cursor_frame(struct wl_listener *listener, __attribute__((unused)) void *data) {
  struct ews_server *server = wl_container_of(listener, server, cursor_frame);
  wlr_seat_pointer_notify_frame(server->seat);
}
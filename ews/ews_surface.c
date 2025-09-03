/* ews_surface.c --- Emacs wayland server surface management

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

   XDG surface management, window focus, mapping/unmapping, and layout.
*/

#define _POSIX_C_SOURCE 200112L

#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>

#include "ewp-protocol.h"
#include "ews_ewp.h"
#include "ews_internal.h"
#include "ews_surface.h"

void focus_surface(struct ews_surface *ews_surface, const struct wlr_surface *surface) {
  if (ews_surface == NULL) {
    return;
  }
  struct ews_server *server = ews_surface->server;
  struct wlr_seat *seat = server->seat;
  const struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    return;
  }
  if (prev_surface) {
    struct wlr_xdg_surface *previous =
        wlr_xdg_surface_try_from_wlr_surface(seat->keyboard_state.focused_surface);
    if (previous != NULL) {
      assert(previous->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);
      wlr_xdg_toplevel_set_activated(previous->toplevel, false);
    }
  }
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  wlr_xdg_toplevel_set_activated(ews_surface->xdg_toplevel, true);
  if (keyboard != NULL) {
    wlr_seat_keyboard_notify_enter(seat, ews_surface->xdg_toplevel->base->surface,
                                   keyboard->keycodes, keyboard->num_keycodes,
                                   &keyboard->modifiers);
  }

  ewp_surface_send_focus(ews_surface->ewp_surface);
}

void layout_surface(struct ews_surface *surface) {
  if (surface->scene_tree == NULL) {
    struct wlr_scene_tree *parent_tree = surface->target_output
                                             ? &surface->target_output->scene->tree
                                             : &surface->server->scene->tree;

    surface->scene_tree =
        wlr_scene_xdg_surface_create(parent_tree, surface->xdg_toplevel->base);
    surface->scene_tree->node.data = surface;
    surface->xdg_surface->data = surface->scene_tree;
  } else {
    wlr_scene_node_set_enabled(&surface->scene_tree->node, true);
  }

  wlr_xdg_toplevel_set_size(surface->xdg_toplevel, surface->width, surface->height);
  wlr_scene_node_set_position(&surface->scene_tree->node, surface->x, surface->y);
}

static void xdg_toplevel_map(struct wl_listener *listener, __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, map);

  surface->mapped = true;

  if (surface->width != 0 && surface->height != 0) {
    layout_surface(surface);
  }
}

static void xdg_toplevel_unmap(struct wl_listener *listener,
                               __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, unmap);

  wlr_log(WLR_ERROR, "Attention: Someone called xdg_toplevel_unmap for %p", (void *)surface);
}

static void xdg_toplevel_destroy(struct wl_listener *listener,
                                 __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, destroy);
  wl_resource_destroy(surface->ewp_surface);
}

static void xdg_toplevel_request_maximize(struct wl_listener *listener,
                                          __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, request_maximize);
  wlr_xdg_surface_schedule_configure(surface->xdg_toplevel->base);
}

static void xdg_toplevel_request_fullscreen(struct wl_listener *listener,
                                            __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, request_fullscreen);
  wlr_xdg_surface_schedule_configure(surface->xdg_toplevel->base);
}

static void xdg_toplevel_set_title(struct wl_listener *listener,
                                   __attribute__((unused)) void *data) {
  struct ews_surface *surface = wl_container_of(listener, surface, set_title);

  ewp_surface_send_update_title(surface->ewp_surface, surface->xdg_toplevel->title);
}

void server_new_xdg_surface(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  if (server->layout_resource == NULL) {
    return;
  }

  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent =
        wlr_xdg_surface_try_from_wlr_surface(xdg_surface->popup->parent);
    struct wlr_scene_tree *parent_tree = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_tree, xdg_surface);
    return;
  }

  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);

  struct ews_surface *surface = calloc(1, sizeof(struct ews_surface));
  wl_list_insert(&server->surfaces, &surface->link);
  surface->server = server;
  surface->xdg_surface = xdg_surface;
  surface->xdg_toplevel = xdg_surface->toplevel;

  struct wl_client *client = wl_resource_get_client(server->layout_resource);
  struct wl_resource *resource = wl_resource_create(client, &ewp_surface_interface, 1, 0);
  wl_resource_set_implementation(resource, &ewp_surface_implementation, surface,
                                 ewp_surface_destroy);
  surface->ewp_surface = resource;

  pid_t pid;
  wl_client_get_credentials(xdg_surface->client->client, &pid, 0, 0);
  wlr_log(WLR_DEBUG, "New toplevel XDG surface app_id=%s title=%s pid=%d",
          xdg_surface->toplevel->app_id, xdg_surface->toplevel->title, pid);
  ewp_layout_send_new_surface(server->layout_resource, resource, xdg_surface->toplevel->app_id,
                              pid);

  surface->map.notify = xdg_toplevel_map;
  wl_signal_add(&xdg_surface->surface->events.map, &surface->map);
  surface->unmap.notify = xdg_toplevel_unmap;
  wl_signal_add(&xdg_surface->surface->events.unmap, &surface->unmap);
  surface->destroy.notify = xdg_toplevel_destroy;
  wl_signal_add(&xdg_surface->events.destroy, &surface->destroy);

  struct wlr_xdg_toplevel *toplevel = xdg_surface->toplevel;
  surface->request_maximize.notify = xdg_toplevel_request_maximize;
  wl_signal_add(&toplevel->events.request_maximize, &surface->request_maximize);
  surface->request_fullscreen.notify = xdg_toplevel_request_fullscreen;
  wl_signal_add(&toplevel->events.request_fullscreen, &surface->request_fullscreen);
  surface->set_title.notify = xdg_toplevel_set_title;
  wl_signal_add(&toplevel->events.set_title, &surface->set_title);
}
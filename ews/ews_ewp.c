/* ews_ewp.c --- Emacs wayland server protocol implementation

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

   Custom EWP (Emacs Wayland Protocol) implementation for communication
   between the C compositor and Emacs.
*/

#define _POSIX_C_SOURCE 200112L

#include <stdlib.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>

#include "ewp-protocol.h"
#include "ews_ewp.h"
#include "ews_internal.h"
#include "ews_output.h"

static void ewp_surface_handle_layout(__attribute__((unused)) struct wl_client *client,
                                      struct wl_resource *resource, uint32_t x, uint32_t y,
                                      uint32_t width, uint32_t height, uint32_t output_id) {
  wlr_log(WLR_DEBUG, "Laying out surface x=%d y=%d width=%d height=%d output_id=%d", x, y,
          width, height, output_id);

  struct ews_surface *surface = wl_resource_get_user_data(resource);
  struct ews_output *target_output = find_output_by_index(surface->server, output_id);

  if (!target_output) {
    wlr_log(WLR_ERROR, "Output ID %d not found, using primary output", output_id);
    target_output = wl_container_of(surface->server->outputs.next, target_output, link);
  }

  struct wlr_box output_box;
  wlr_output_layout_get_box(surface->server->output_layout, target_output->wlr_output,
                            &output_box);

  wlr_log(WLR_DEBUG, "Target output name=%s box=(x=%d y=%d w=%d h=%d)",
          target_output->wlr_output && target_output->wlr_output->name
              ? target_output->wlr_output->name
              : "unknown",
          output_box.x, output_box.y, output_box.width, output_box.height);

  surface->x = (int)x;
  surface->y = (int)y;
  surface->width = width;
  surface->height = height;
  surface->target_output = target_output;

  wlr_log(WLR_DEBUG, "Computed scene pos=(%d,%d) size=(%dx%d) for output_id=%d", surface->x,
          surface->y, surface->width, surface->height, output_id);
  if (surface->mapped) {
    layout_surface(surface);
  }
}

static void ewp_surface_handle_hide(__attribute__((unused)) struct wl_client *client,
                                    struct wl_resource *resource) {
  struct ews_surface *surface = wl_resource_get_user_data(resource);
  if (surface->scene_tree != NULL) {
    wlr_scene_node_set_enabled(&surface->scene_tree->node, false);
  }
}

static void ewp_surface_handle_focus(__attribute__((unused)) struct wl_client *client,
                                     struct wl_resource *resource) {
  struct ews_surface *surface = wl_resource_get_user_data(resource);
  focus_surface(surface, surface->xdg_toplevel->base->surface);
}

static void ewp_surface_handle_client_destroy(__attribute__((unused)) struct wl_client *client,
                                              struct wl_resource *resource) {
  struct ews_surface *surface = wl_resource_get_user_data(resource);
  wl_signal_emit(&surface->xdg_surface->events.destroy, surface->xdg_surface);
}

const struct ewp_surface_interface ewp_surface_implementation = {
    .layout = ewp_surface_handle_layout,
    .hide = ewp_surface_handle_hide,
    .focus = ewp_surface_handle_focus,
    .destroy = ewp_surface_handle_client_destroy,
};

void ewp_surface_destroy(struct wl_resource *resource) {
  ewp_surface_send_destroy(resource);
  struct ews_surface *surface = wl_resource_get_user_data(resource);

  wl_list_remove(&surface->link);
  wl_list_remove(&surface->map.link);
  wl_list_remove(&surface->unmap.link);
  wl_list_remove(&surface->destroy.link);
  wl_list_remove(&surface->request_maximize.link);
  wl_list_remove(&surface->request_fullscreen.link);
  wl_list_remove(&surface->set_title.link);

  free(surface);
}

void ewp_layout_handle_bind(struct wl_client *client, void *data, uint32_t version,
                            uint32_t id) {
  struct ewp_layout *layout = data;

  if (layout->server->layout_resource != NULL) {
    wlr_log(WLR_ERROR, "Error: 2nd layout client tried to connect but only one allowed");
    return;
  }

  struct wl_resource *resource = wl_resource_create(client, &ewp_layout_interface, version, id);

  layout->server->layout_resource = resource;
}
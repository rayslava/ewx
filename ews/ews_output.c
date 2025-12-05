/* ews_output.c --- Emacs wayland server output management

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

   Output (monitor/display) management, frame rendering, and multi-output support.
*/

#define _POSIX_C_SOURCE 200112L

#include <stdlib.h>
#include <time.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/util/log.h>

#include "ews_internal.h"
#include "ews_output.h"

static void output_frame(struct wl_listener *listener, __attribute__((unused)) void *data) {
  struct ews_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->scene;

  struct wlr_scene_output *scene_output = wlr_scene_get_scene_output(scene, output->wlr_output);

  if (!scene_output) {
    scene_output = wlr_scene_output_create(scene, output->wlr_output);
    if (!scene_output) {
      wlr_log(WLR_DEBUG, "Scene output is NULL, skipping frame");
      return;
    }
    wlr_log(WLR_INFO, "Created scene output for %s", output->wlr_output->name);
  }

  struct wlr_box obox;
  wlr_output_layout_get_box(output->server->output_layout, output->wlr_output, &obox);
  wlr_log(WLR_DEBUG, "Committing scene for output %s layout box=(%d,%d %dx%d)",
          output->wlr_output->name ? output->wlr_output->name : "unknown", obox.x, obox.y,
          obox.width, obox.height);

  wlr_scene_output_commit(scene_output, NULL);

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(scene_output, &now);
}

static void output_destroy(struct wl_listener *listener, __attribute__((unused)) void *data) {
  struct ews_output *output = wl_container_of(listener, output, destroy);

  if (output->scene) {
    wlr_scene_node_destroy(&output->scene->tree.node);
  }

  wl_list_remove(&output->frame.link);
  wl_list_remove(&output->destroy.link);
  wl_list_remove(&output->link);
  free(output);
}

void server_new_output(struct wl_listener *listener, void *data) {
  struct ews_server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  if (!wl_list_empty(&wlr_output->modes)) {
    struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    wlr_output_set_mode(wlr_output, mode);
    wlr_output_enable(wlr_output, true);
    if (!wlr_output_commit(wlr_output)) {
      return;
    }
  }

  struct ews_output *output = calloc(1, sizeof(struct ews_output));
  output->wlr_output = wlr_output;
  output->server = server;

  output->scene = wlr_scene_create();
  if (!output->scene) {
    wlr_log(WLR_ERROR, "Failed to create scene for output");
    free(output);
    return;
  }

  uint32_t output_index = 0;
  struct ews_output *existing;
  wl_list_for_each(existing, &server->outputs, link) { output_index++; }

  wlr_log(WLR_INFO, "New output '%s' assigned index %d",
          wlr_output->name ? wlr_output->name : "unknown", output_index);

  output->frame.notify = output_frame;
  wl_signal_add(&wlr_output->events.frame, &output->frame);

  output->destroy.notify = output_destroy;
  wl_signal_add(&wlr_output->events.destroy, &output->destroy);

  wl_list_insert(server->outputs.prev, &output->link);

  wlr_output_layout_add_auto(server->output_layout, wlr_output);
}

struct ews_output *find_output_by_index(struct ews_server *server, uint32_t output_index) {
  struct ews_output *output;
  uint32_t index = 0;
  wl_list_for_each(output, &server->outputs, link) {
    if (index == output_index) {
      return output;
    }
    index++;
  }
  return NULL;
}

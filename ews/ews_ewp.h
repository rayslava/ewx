/* ews_ewp.h --- Emacs wayland server protocol implementation

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
*/

#ifndef EWS_EWP_H
#define EWS_EWP_H

#include "ews_types.h"

/* EWP protocol interface implementations */
extern const struct ewp_surface_interface ewp_surface_implementation;

/* EWP protocol handlers */
void ewp_layout_handle_bind(struct wl_client *client, void *data, uint32_t version,
                            uint32_t id);
void ewp_surface_destroy(struct wl_resource *resource);

#endif /* EWS_EWP_H */
/* ews_output.h --- Emacs wayland server output management

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

#ifndef EWS_OUTPUT_H
#define EWS_OUTPUT_H

#include "ews_types.h"

/* Output event handlers */
void server_new_output(struct wl_listener *listener, void *data);

/* Output utility functions */
struct ews_output *find_output_by_index(struct ews_server *server, uint32_t output_index);

#endif /* EWS_OUTPUT_H */
/* ews_input.h --- Emacs wayland server input management

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

#ifndef EWS_INPUT_H
#define EWS_INPUT_H

#include "ews_types.h"

/* Input event handlers */
void server_new_input(struct wl_listener *listener, void *data);
void seat_request_cursor(struct wl_listener *listener, void *data);
void seat_request_set_selection(struct wl_listener *listener, void *data);

#endif /* EWS_INPUT_H */
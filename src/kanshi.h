/*
 * Copyright (C) 2017-2019 emersion
 * Copyright (C) 2019 cyclopsian

 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * Parts of this file are taken from emersion/kanshi:
 * https://github.com/emersion/kanshi/blob/38d27474b686fcc8324cc5e454741a49577c0988/include/config.h
 * https://github.com/emersion/kanshi/blob/38d27474b686fcc8324cc5e454741a49577c0988/include/parser.h
 */

#ifndef WDISPLAYS_KANSHI_H
#define WDISPLAYS_KANSHI_H

#include <stdbool.h>
#include <wayland-client.h>

enum kanshi_output_field {
  KANSHI_OUTPUT_ENABLED = 1 << 0,
  KANSHI_OUTPUT_MODE = 1 << 1,
  KANSHI_OUTPUT_POSITION = 1 << 2,
  KANSHI_OUTPUT_SCALE = 1 << 3,
  KANSHI_OUTPUT_TRANSFORM = 1 << 4,
};

struct kanshi_profile_output {
  char *name;
  unsigned int fields; // enum kanshi_output_field
  struct wl_list link;

  bool enabled;
  struct {
    int width, height;
    int refresh; // mHz
  } mode;
  struct {
    int x, y;
  } position;
  float scale;
  enum wl_output_transform transform;
};


struct kanshi_profile_command {
  struct wl_list link;
  char *command;
};

struct kanshi_profile {
  struct wl_list link;
  char *name;
  // Wildcard outputs are stored at the end of the list
  struct wl_list commands;
  struct wl_list outputs;
};

struct kanshi_config {
  struct wl_list profiles;
};

/*
 * Loads the kanshi config from the given file.
 */
struct kanshi_config *kanshi_parse_config(const char *path);

/*
 * Saves the kanshi config to the given file.
 */
void kanshi_save_config(const char *path, struct kanshi_config *config);

/*
 * Destroys the config structure.
 */
void kanshi_destroy_config(struct kanshi_config *config);

#endif

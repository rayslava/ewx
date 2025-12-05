#ifndef EWS_SCREENCOPY_H
#define EWS_SCREENCOPY_H

#include <wlr/types/wlr_screencopy_v1.h>

struct ews_server;

struct ews_screencopy_manager {
    struct wlr_screencopy_manager_v1 *manager;
};

bool ews_screencopy_manager_init(struct ews_screencopy_manager *manager,
                                struct wl_display *display,
                                struct wlr_renderer *renderer);
void ews_screencopy_manager_finish(struct ews_screencopy_manager *manager);

#endif // EWS_SCREENCOPY_H
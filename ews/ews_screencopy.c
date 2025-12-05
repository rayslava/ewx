#include "ews_screencopy.h"
#include "ews_types.h"
#include <wlr/util/log.h>

bool ews_screencopy_manager_init(struct ews_screencopy_manager *manager,
        struct wl_display *display, struct wlr_renderer *renderer) {
    manager->manager = wlr_screencopy_manager_v1_create(display);
    if (!manager->manager) {
        wlr_log(WLR_ERROR, "Failed to create wlr_screencopy_manager_v1");
        return false;
    }

    wlr_log(WLR_INFO, "wlr-screencopy-unstable-v1 initialized");
    return true;
}

void ews_screencopy_manager_finish(struct ews_screencopy_manager *manager) {
    if (!manager || !manager->manager) {
        return;
    }

    // wlroots manages the lifecycle, we just need to clear our reference
    manager->manager = NULL;
}
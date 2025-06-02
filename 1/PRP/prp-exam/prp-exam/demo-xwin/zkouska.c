#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "exam_utils.h"
#include "save_jpeg.h"
#include "xwin_sdl.h"

#define ERROR_FILE 101
#define ERROR_PROCESS 102
#define RGB 3

typedef struct {
    int x1;
    int y1;
    int w;
    int h;
    int x2;
    int y2;
} img_cutouts;

bool process_file(FILE *f, int image_w, int image_h, unsigned char *img, bool anim);
bool copy(int image_w, int image_h, unsigned char *img, img_cutouts cfg);
bool swap(int image_w, int image_h, unsigned char *img, img_cutouts cfg);

int main(int argc, char *argv[]) {
    int ret = EXIT_SUCCESS;
    const char *srcfile = argc > 1 ? argv[1] : NULL;
    const char *txtfile = argc > 2 ? argv[2] : NULL;
    const char *svfile = argc > 3 ? argv[3] : NULL;
    const bool anim = argc > 4 && strcmp(argv[4], "--anim") == 0;
    my_assert(srcfile && txtfile && svfile, __func__, __LINE__, __FILE__);
    int image_w, image_h;
    unsigned char *img = xwin_load_image(srcfile, &image_w, &image_h);
    my_assert(img != NULL, __func__, __LINE__, __FILE__);
    if (anim) {
        xwin_init(image_w, image_h);
    }
    FILE *f = fopen(txtfile, "r");
    if (!f) {
        fprintf(stderr, "ERROR: cannot open file '%s'\n", txtfile);
        ret = ERROR_FILE;
    }
    if (!ret && !process_file(f, image_w, image_h, img, anim)) {
        fprintf(stderr, "ERROR: cannot process file '%s'\n", txtfile);
        ret = ERROR_PROCESS;
    }
    fclose(f);
    if (!ret && svfile) {
        if (strstr(svfile, ".jpg") || strstr(svfile, ".jpeg")) {
            save_image_jpeg(image_w, image_h, img, svfile);
        }
        else {
            save_image_ppm(image_w, image_h, img, svfile);
        }
    }
    if (anim) {
        xwin_close();
    }
    free(img);
    return ret;
}
bool process_file(FILE *f, int image_w, int image_h, unsigned char *img, bool anim) {
    bool ret = true;
    int counter = 0;
    img_cutouts cfg;
    char cmd[3];
    while (!feof(f) && ret) {
        int r = fscanf(f, "%2s %d %d %d %d %d %d\n", cmd, &cfg.x1, &cfg.y1, &cfg.w, &cfg.h, &cfg.x2, &cfg.y2);
        if (r == 7 && strcmp(cmd, "cp") == 0) {
            ret = copy(image_w, image_h, img, cfg);
        }
        else if (r == 7 && strcmp(cmd, "sw") == 0) {
            ret = swap(image_w, image_h, img, cfg);
        }
        else {
            ret = false;
            break;
        }
        counter += 1;
        if (anim && (counter % 10) == 0) {
            xwin_redraw(image_w, image_h, img);
            delay(10);
        }
    }
    return ret;
}
bool copy(int image_w, int image_h, unsigned char *img, img_cutouts cfg) {
    bool ret = true;
    for (int xi = 0; xi < cfg.w; ++xi) {
        for (int yi = 0; yi < cfg.h; ++yi) {
            const int sx = cfg.x1 + xi;
            const int sy = cfg.y1 + yi;
            const int dx = cfg.x2 + xi;
            const int dy = cfg.y2 + yi;
            if (sx >= 0 && sx < image_w && sy >= 0 && sy < image_h 
                && dx >= 0 && dx < image_w && dy >= 0 && dy < image_h) 
            {
                for (int i = 0; i < RGB; ++i) {
                    img[(dy * image_w + dx) * RGB + i] = img[(sy * image_w + sx) * RGB + i];
                }
            }
            else {
                fprintf(stderr, "ERROR: copy failed\n");
                return false;
            }
        }
    }
    return ret;
}
bool swap(int image_w, int image_h, unsigned char *img, img_cutouts cfg) {
    bool ret = true;
    for (int xi = 0; xi < cfg.w; ++xi) {
        for (int yi = 0; yi < cfg.h; ++yi) {
            const int sx = cfg.x1 + xi;
            const int sy = cfg.y1 + yi;
            const int dx = cfg.x2 + xi;
            const int dy = cfg.y2 + yi;
            if (sx >= 0 && sx < image_w && sy >= 0 && sy < image_h 
                && dx >= 0 && dx < image_w && dy >= 0 && dy < image_h) 
            {
                for (int i = 0; i < RGB; ++i) {
                    unsigned char tmp = img[(dy * image_w + dx) * RGB + i];
                    img[(dy * image_w + dx) * RGB + i] = img[(sy * image_w + sx) * RGB + i];
                    img[(sy * image_w + sx) * RGB + i] = tmp;
                }
            }
            else {
                fprintf(stderr, "ERROR: swap failed\n");
                return false;
            }
        }
    }
    return ret;
}

/*
 * File name: texam.c
 * Date:      2017/01/16 08:43
 * Author:    Jan Faigl
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "exam_utils.h"
#include "xwin_sdl.h"
#include "save_jpeg.h"

bool process_file(FILE *fd, unsigned char *img, int image_w, int image_h, bool anim);
bool copy(unsigned char *img, int image_w, int image_h, int x1, int y1, int w, int h, int x2, int y2);
bool swap(unsigned char *img, int image_w, int image_h, int x1, int y1, int w, int h, int x2, int y2);

int main(int argc, char *argv[]) 
{
   int ret = EXIT_SUCCESS;
   //1st read arguments
   const char *fimage = argc > 1 ? argv[1] : NULL;
   const char *fcmds = argc > 2 ? argv[2] : NULL;
   const char *fout = argc > 3 ? argv[3] : NULL;
   const bool anim = argc > 4 && strcmp(argv[4], "--anim") == 0;

   my_assert(fimage != NULL && fcmds != NULL && fout != NULL, __func__, __LINE__, __FILE__);

   //2nd read image
   int image_w, image_h;
   unsigned char *img = xwin_load_image(fimage, &image_w, &image_h);
   my_assert(img != NULL, __func__, __LINE__, __FILE__);

   if (anim) {
      xwin_init(image_w, image_h);  
   }

   //3rd process cmds
   FILE *fd = fopen(fcmds, "r");
   if (fd == NULL) {
      fprintf(stderr, "ERROR: cannot open file '%s'\n", fcmds);
      ret = 101;
   }

   if (!ret && !process_file(fd, img, image_w, image_h, anim)) {
      fprintf(stderr, "ERROR: processing file '%s'\n", fcmds);
      ret = 102;
   }
   if (fd) {
      fclose(fd);
   }

   //4th save output
   if (!ret && fout) {
      if (strstr(fout, ".jpeg") || strstr(fout, ".jpg")) {
         save_image_jpeg(image_w, image_h, img, fout);
      }
      else {
         save_image_ppm(image_w, image_h, img, fout);
      }
   }

   if (anim) {
      xwin_close();
   }
   free(img);
   return ret;
}

bool process_file(FILE *fd, unsigned char *img, int image_w, int image_h, bool anim) {
   bool ret = true;
   int x1, y1, x2, y2;
   int w, h;
   char cmd[3];
   int counter = 0;
   while (ret && !feof(fd)) {
      int r = fscanf(fd, "%2s %d %d %d %d %d %d\n", cmd, &x1, &y1, &w, &h, &x2, &y2);
      if (r == 7 && strcmp(cmd, "cp") == 0) {
         ret = copy(img, image_w, image_h, x1, y1, w, h, x2, y2);
      }
      else if (r == 7 && strcmp(cmd, "sw") == 0) {
         ret = swap(img, image_w, image_h, x1, y1, w, h, x2, y2);
      }
      else {
         ret = false;
         break;
      }
      counter += 1;
      if (anim && (counter % 15) == 0) {
         xwin_redraw(image_w, image_h, img);
         delay(1);
      }
   } // end while
   return ret;
}

bool copy(unsigned char *img, int image_w, int image_h, int x1, int y1, int w, int h, int x2, int y2) {
   bool ret = true;
   for (int xi = 0; xi < w; ++xi) {
      for (int yi = 0; yi < h; ++yi) {
         const int sx = x1 + xi;
         const int sy = y1 + yi;
         const int dx = x2 + xi;
         const int dy = y2 + yi;
         if (sx >= 0 && sx < image_w && sy >=0 && sy < image_h 
         && dx >= 0 && dx < image_w && dy >= 0 && dy < image_h) {
            for (int i = 0; i < 3; ++i) {
               img[(dy * image_w + dx) * 3 + i] = img[(sy * image_w + sx) * 3 + i];
            }
         }
         else {
            fprintf(stderr, "ERROR: copy cmd out of range!\n");
            return false;
         }
      }
   }
   return ret;
}

bool swap(unsigned char *img, int image_w, int image_h, int x1, int y1, int w, int h, int x2, int y2) {
   bool ret = true;
   for (int xi = 0; xi < w; ++xi) {
      for (int yi = 0; yi < h; ++yi) {
         const int sx = x1 + xi;
         const int sy = y1 + yi;
         const int dx = x2 + xi;
         const int dy = y2 + yi;
         if (sx >= 0 && sx < image_w && sy >=0 && sy < image_h 
         && dx >= 0 && dx < image_w && dy >= 0 && dy < image_h) {
            for (int i = 0; i < 3; ++i) {
               unsigned char t = img[(sy * image_w + sx) * 3 + i];
               img[(sy * image_w + sx) * 3 + i] = img[(dy * image_w + dx) * 3 + i];
               img[(dy * image_w + dx) * 3 + i] = t;
            }
         }
         else {
            fprintf(stderr, "ERROR: swap cmd out of range!\n");
            return false;
         }
      }
   }
   return ret;
}

/* end of texam.c */

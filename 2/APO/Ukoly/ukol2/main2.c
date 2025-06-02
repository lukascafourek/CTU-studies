#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EXIT_SUCCESS 0
#define WRONG_INPUT 100
#define ERROR_SOURCE 101
#define ERROR_OUTPUT 102
#define ERROR_READ 103
#define ERROR_WRITE 104
#define ERROR_BRIGHTNESS 105
#define ERROR_TEXTFILE 106
#define ERROR_TEXTFILE_WRITE 107

typedef struct {
    char first[3];
    int width;
    int height;
    int depth;
} txt_info;

typedef struct {
    int a;
    int b;
    int c;
    int d;
    int e;
} brightness_info;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "ERROR: wrong input!\n");
        return WRONG_INPUT;
    }
    char *srcimg = argv[1];
    txt_info info;

    FILE *source = fopen(srcimg, "r");
    if (source == NULL) {
        fprintf(stderr, "ERROR opening source file READ!\n");
        return ERROR_SOURCE;
    }
    int r = fscanf(source, "%2s\n%d\n%d\n%d\n", info.first, &info.width, &info.height, &info.depth);
    if (r != 4) {
        fprintf(stderr, "ERROR: wrong input image info!\n");
        return ERROR_READ;
    }

    FILE *output = fopen("output4.txt", "w");
    if (output == NULL) {
        fprintf(stderr, "ERROR opening output file READ!\n");
        return ERROR_OUTPUT;
    }
    int s = fprintf(output, "%2s\n%d\n%d\n%d\n", info.first, info.width, info.height, info.depth);
    if (s <= 0) {
        fprintf(stderr, "ERROR writing image info into output file!\n");
        return ERROR_OUTPUT;
    }

    source = fopen(srcimg, "rb");
    if (source == NULL) {
        fprintf(stderr, "ERROR opening source file READ BINARY!\n");
        return ERROR_SOURCE;
    }
    unsigned char *img = (unsigned char *)malloc(info.width * info.height * 3 * (sizeof(unsigned char)));
    fseek(source, s, SEEK_SET);
    r = fread(img, 1, info.width * info.height * 3, source);
    if (r <= 0) {
        fprintf(stderr, "ERROR reading binary source file!\n");
        return ERROR_READ;
    }
    fclose(source);

    //unsigned char *txt = (unsigned char *)malloc(info.width * info.height * 3 * (sizeof(unsigned char)));
    for (int i = 0; i < info.width * info.height * 3; ++i) {
        int c = (int)img[i];
        fprintf(output, "%d ", c);
    }
    fclose(output);

    // output = fopen("output2.txt", "w");
    // if (output == NULL) {
    //     fprintf(stderr, "ERROR opening output file READ BINARY!\n");
    //     return ERROR_OUTPUT;
    // }
    // brightness_info b_info = {.a = 0, .b = 0, .c = 0, .d = 0, .e = 0};
    // int brightness;
    // unsigned char rgb[3];
    // fseek(output, s, SEEK_SET);
    // for (int i = 0; i < info.height; ++i) {
    //     for (int j = 0; j < info.width; ++j) {
    //         if ((i == 0 || i == info.height - 1) && j < info.width) {
    //             for (int k = 0; k < 3; ++k) {
    //                 rgb[k] = img[(i * info.width + j) * 3 + k];
    //             }
    //             brightness = round(0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]);
    //         } else if (j == 0 || j == info.width - 1) {
    //             for (int k = 0; k <3; ++k) {
    //                 rgb[k] = img[(i * info.width + j) * 3 + k];
    //             }
    //             brightness = round(0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]);
    //         } else {
    //             for (int k = 0; k < 3; ++k) {
    //                 rgb[k] = 5 * img[(i * info.width + j) * 3 + k] - img[(i * info.width + j) * 3 + k - 3] 
    //                 - img[(i * info.width + j) * 3 + k + 3] - img[((i - 1) * info.width + j) * 3 + k] - img[((i + 1) * info.width + j) * 3 + k];
    //                 //rgb[k] = img[(i * info.width + j) * 3 + k];
    //                 if (rgb[k] > 255) {
    //                     rgb[k] = 255;
    //                 } else if (rgb[k] < 0) {
    //                     rgb[k] = 0;
    //                 }
    //             }
    //             brightness = round(0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]);
    //         }
    //         r = fwrite(rgb, 1, 3, output);
    //         if (r != 3) {
    //             fprintf(stderr, "ERROR writing binary image into output file!\n");
    //             return ERROR_OUTPUT;
    //         }
    //         if (brightness >= 0 && brightness <= 50) {
    //             b_info.a += 1;
    //         } else if (brightness > 50 && brightness <= 101) {
    //             b_info.b += 1;
    //         } else if (brightness > 101 && brightness <= 152) {
    //             b_info.c += 1;
    //         } else if (brightness > 152 && brightness <= 203) {
    //             b_info.d += 1;
    //         } else if (brightness > 203 && brightness <= 255) {
    //             b_info.e += 1;
    //         } else {
    //             fprintf(stderr, "ERROR: invalid brightness number!\n");
    //             return ERROR_BRIGHTNESS;
    //         }
    //     }
    // }
    // fclose(output);

    // FILE *outtxt = fopen("output.txt", "w");
    // if (outtxt == NULL) {
    //     fprintf(stderr, "ERROR opening text output file!\n");
    //     return ERROR_TEXTFILE;
    // }
    // r = fprintf(outtxt, "%d %d %d %d %d", b_info.a, b_info.b, b_info.c, b_info.d, b_info.e);
    // if (r <= 0) {
    //     fprintf(stderr, "ERROR writing in output text file!\n");
    //     return ERROR_TEXTFILE_WRITE;
    // }
    // fclose(outtxt);

    return EXIT_SUCCESS;
}

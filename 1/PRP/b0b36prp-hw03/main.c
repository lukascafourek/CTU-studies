#include <stdio.h>
#include <stdlib.h>

#define MIN_VALUE 3
#define MAX_VALUE 69
#define EXIT_SUCCESS 0
#define ERROR_INPUT 100
#define ERROR_RANGE 101
#define ERROR_WIDTH 102
#define ERROR_FENCE 103

int read_input(int *width, int *height, int *fence);
void print_error(int ret);
void print_house_fence(int width, int height, int fence);
void roof(int x, int y);
void house_fence(int i, int j, int width, int height, int fence);
void house_floor(int j, int width);
void house_floor_fence(int i, int width, int height, int fence);
void house_inside_fence(int i, int width, int height, int fence);
void house_inside(int i, int width, int height, int fence);
void ceiling(int i, int width);
void insides_even(int j, int width);
void insides_odd(int j, int width);
void walls(int j, int width);
void insides(int j, int width);

int main(int argc, char *argv[]) {
  int height, width, fence;
  int ret = read_input(&width, &height, &fence);
  if (ret == EXIT_SUCCESS) {
    print_house_fence(width, height, fence);
  }
  else {
    print_error(ret);
  }
  return ret;
}
int read_input(int *width, int *height, int *fence) {   // this reads input
  int ret = EXIT_SUCCESS, r, s;
  r = scanf("%d%d", width, height);
  if (r != 2) {
    return ERROR_INPUT;
  }
  if (*width == *height) {
    s = scanf(" %d", fence);
  }
  else {
    s = 1;
    *fence = 1;
  }
  if (s != 1) {
    ret = ERROR_INPUT;
  }
  else if (*width < 3 || *height < 3 || *width > 69 || *height > 69) {
    ret = ERROR_RANGE;
  }
  else if (*width % 2 != 1) {
    ret = ERROR_WIDTH;
  }
  else if (*fence <= 0 || *fence >= *height) {
    ret = ERROR_FENCE;
  }
  return ret;
}
void print_error(int ret) {   // this returns error
  switch (ret) {
    case ERROR_INPUT:
      fprintf(stderr, "Error: Chybny vstup!\n");
      break;
    case ERROR_RANGE:
      fprintf(stderr, "Error: Vstup mimo interval!\n");
      break;
    case ERROR_WIDTH:
      fprintf(stderr, "Error: Sirka neni liche cislo!\n");
      break;
    case ERROR_FENCE:
      fprintf(stderr, "Error: Neplatna velikost plotu!\n");
      break;
  }
}
// this function below prints house with or without fence via other functions
void print_house_fence(int width, int height, int fence) {
  int x = width / 2 + 1, y = width / 2 + 1, z = width / 2 + 1;
  for (int i = 0; i < width - z; ++i) {
    roof(x, y);
  x--;
  y++;
  putchar('\n');
  }
  for (int i = 0; i < height; ++i) {
    house_floor_fence(i, width, height, fence);
    house_inside_fence(i, width, height, fence);
    ceiling(i, width);
    house_inside(i, width, height, fence);
  }
}
void roof(int x, int y) {   // this prints roof
  for (int j = 0; j < y; ++j) {
    if (j == x - 1 || j == y - 1) {
      putchar('X');
    }
    else {
      putchar(' ');
    }
  }
}
// this below prints floor and bottom of the fence
void house_floor_fence(int i, int width, int height, int fence) {
  if (i == height - 1) {
    for (int j = 0; j < width + fence; ++j) {
      house_floor(j, width);
      if (fence != 1) {
        house_fence(i, j, width, height, fence);
      }
    }
    putchar('\n');
  }
}
void house_floor(int j, int width) { // this prints floor
  if (j < width) {
    putchar('X');
  }
}
// this below prints inner and outer fence
void house_fence(int i, int j, int width, int height, int fence) {
  if ((i == height - 1 || i == height - fence) && j >= width) {
    if (fence % 2 == 0) {
      if (j % 2 == 0) {
        putchar('|');
      }
      else {
        putchar('-');
      }
    }
    else {
      if (j % 2 != 0) {
        putchar('|');
      }
      else {
        putchar('-');
      }
    }
  }
  else if ((i < height - 1 || i > height - fence) && j >= width) {
    if (fence % 2 == 0) {
      if (j % 2 == 0) {
        putchar('|');
      }
      else {
        putchar(' ');
      }
    }
    else {
      if (j % 2 != 0) {
        putchar('|');
      }
      else {
        putchar(' ');
      }
    }
  }
}
// this below prints interior, walls and fence via other functions
void house_inside_fence(int i, int width, int height, int fence) {
  if (i >= height - fence && i < height - 1) {
    if (i % 2 == 0) {
      for (int j = 0; j < width + fence; ++j) {
        walls(j, width);
        insides_even(j, width);
        house_fence(i, j, width, height, fence);
      }
    }
    else if (i % 2 !=0) {
      for (int j = 0; j < width + fence; ++j) {
        walls(j, width);
        insides_odd(j, width);
        house_fence(i, j, width, height, fence);
      }
    }
    putchar('\n');
  }
}
void ceiling(int i, int width) {  // this prints ceiling
  if (i == 0) {
    for (int j = 0; j < width; ++j) {
      putchar('X');
    }
    putchar('\n');
  }
}
// this below prints interior and walls without fence via other functions
void house_inside(int i, int width, int height, int fence) {
  if (i > 0 && i < height - fence) {
    if (i % 2 == 0) {
      for (int j = 0; j < width; ++j) {
        walls(j, width);
        if (fence != 1) {
          insides_even(j, width);
        }
        else {
          insides(j, width);
        }
      }
    }
    else if (i % 2 != 0) {
      for (int j = 0; j < width; ++j) {
        walls(j, width);
        if (fence != 1) {
          insides_odd(j, width);
        }
        else {
          insides(j, width);
        }
      }
    }
    putchar('\n');
  }
}
void walls(int j, int width) {    // this prints walls
  if (j == 0 || j == width - 1) {
    putchar('X');
  }
}
// this below prints interior on even rows if the house is with fence
void insides_even(int j, int width) {
  if (j > 0 && j < width - 1 && j % 2 == 0) {
    putchar('o');
  }
  else if (j > 0 && j < width - 1 && j % 2 != 0) {
    putchar('*');
  }
}
// this below prints interior on odd rows if the house is with fence
void insides_odd(int j, int width) {
  if (j > 0 && j < width - 1 && j % 2 != 0) {
    putchar('o');
  }
  else if (j > 0 && j < width - 1 && j % 2 == 0) {
    putchar('*');
  }
}
// this below prints interior if the house is without fence
void insides(int j, int width) {
  if (j > 0 && j < width - 1) {
    putchar(' ');
  }
}

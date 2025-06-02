#include <stdio.h>
#include <stdlib.h>

#define INIT_INPUT_SIZE 10

typedef enum
{
  OK = 0,            	// OK
  NO_PRINT = 1,      	// NO_PRINT
  ALLOC_ERROR = 101, 	// Error: Chyba alokace!
  FILE_OPEN_ERROR = 102, // Error: Chyba otevirani souboru!
  FILE_READ_ERROR = 103, // Error: Chyba cteni souboru!
} ErrorType;

int arr_len(char *pattern)
{
  int len = 0;
  while (pattern[len] != '\0')
  {
    len += 1;
  }
  return len;
}
int choose_line(char *line, char *pattern)
{
  int len = arr_len(pattern);
  int line_len = arr_len(line);
  if (line_len < len)
  {
    return 0;
  }
  for (size_t i = 0; i < line_len; i++)
  {
    if (line[i] == pattern[0])
    {
      int j = 1;
      while (j < len && line[i + j] == pattern[j])
      {
        j += 1;
      }
      if (j == len)
      {
        return 1;
      }
    }
  }
  return 0;
}
int read_line(FILE *f, char *line)
{
  int success = 1;
  int failed = 0;
  char letter = 'a';
  int i = 0;
  while (1)
  {
    letter = fgetc(f);
    if (letter == EOF)
    {
      return failed;
    }
    if (letter == '\n')
    {
      break;
    }
    else
    {
      line[i] = letter;
    }
    i += 1;
  }
  line[i] = '\0';
  return success;
}
int main(int argc, char *argv[])
{
  char *pattern = argv[1];
  char *filename = argv[2];
  size_t buffer = INIT_INPUT_SIZE * 4;
  FILE *f = fopen(filename, "r");
  if (f == NULL)
  {
    fprintf(stderr, "Error: Chyba otevirani souboru!");
    exit(FILE_OPEN_ERROR);
  }
  char *line = calloc(buffer, sizeof(char));
  int line_ret = read_line(f, line);
  int i = 0;
  int num_prints = 0;
  while (line_ret)
  {
    if (choose_line(line, pattern))
    {
      printf("%s\n", line);
      num_prints += 1;
    }
    line_ret = read_line(f, line);
    i += 1;
  }
  free(line);
  fclose(f);
  if (num_prints)
  {
    exit(OK);
  }
  else
  {
    exit(NO_PRINT);
  }
}

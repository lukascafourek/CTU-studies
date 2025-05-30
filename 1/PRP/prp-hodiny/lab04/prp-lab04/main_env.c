#include <stdio.h>

int main(int argc, char **argv, char **envp) 
{
   printf("Number of arguments %d\n", argc);
   printf("Size of argv %lu\n", sizeof(argv)); /* return size of char** */
   printf("Size of envp %lu\n", sizeof(envp));
   printf("Arguments:\n");
   for (int i = 0; i < argc; ++i) {
      printf("argv[%d]='%s'\n", i, argv[i]);
   }
   printf("Environment:\n");
   while (*envp != NULL) {
      printf("Environment: '%s'\n", *envp);
      envp++;
   }
   return 0;
}

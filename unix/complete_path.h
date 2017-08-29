#ifndef COMPLETE_PATH_H_INCLUDED
#define COMPLETE_PATH_H_INCLUDED
#ifdef __cplusplus
extern "C"
{
#endif


int complete_path(char *basepath, char *path,
 char ***suffixes, int *common_suffix_len);

#define COMPLETE_PATH_EARGS   -4
#define COMPLETE_PATH_EBASE   -3
#define COMPLETE_PATH_ENOMEM  -2
#define COMPLETE_PATH_ODERRNO -1 /* opendir() failed */


void complete_path_free(char **suffixes);

char *complete_path_strerror(const int error_code);


#ifdef __cplusplus
}
#endif
#endif /* not included yet */

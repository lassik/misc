// One URL that gives a Content-Disposition header is:
// http://www.vim.org/scripts/download_script.php?src_id=10872

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <curl/curl.h>

static const char safechars[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789"
    "_-+.,= ";

static CURL *curl;
static char *g_filename;

static void die(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static const char *str_caseprefix(const char *str, const char *prefix)
{
    size_t lenstr = strlen(str);
    size_t lenprefix = strlen(prefix);

    if (lenstr < lenprefix)
        return 0;
    if (strncasecmp(str, prefix, lenprefix))
        return 0;
    return str + lenprefix;
}

/*
 * The Content-Disposition header is specified in RFC 2616 section
 * 19.5.1. The full syntax is complex and we don't have adequate
 * parsing tools at hand so let's be sloppy and parse it as follows.
 *
 *     Content-Disposition:<whatever1>filename=<whatever2>
 *
 * where <whatever2> is the filename (with quotes, backslashes and
 * other "weird" chars removed). Here's an example:
 *
 *     Content-Disposition: attachment; filename=pythoncomplete.vim
 */
static char *get_content_disposition_filename(const char *str)
{
    const char disposition[] = "Content-Disposition:";
    const char filename_eq[] = "filename=";
    char *filename, *dst;

    if (!(str = str_caseprefix(str, disposition)))
        return 0;
    if (!(str = strcasestr(str, filename_eq)))
        return 0;
    str += strlen(filename_eq);
    if (!(dst = filename = malloc(strlen(str))))
        return 0;
    for (; *str; str++) {
        if (strchr(safechars, *str)) {
            *dst++ = *str;
        }
    }
    while ((dst > filename) && (dst[-1] == ' '))
        dst--;
    *dst = 0;
    return filename;
}

static void handle_content_disposition(const char *str)
{
    char *filename;

    if ((filename = get_content_disposition_filename(str))) {
        free(g_filename);
        g_filename = filename;
    }
}

static size_t header_cb(char *buf, size_t size, size_t nmemb, void *userdata)
{
    const size_t len = size * nmemb;
    char *url = 0;
    char *limit;

    (void)userdata;
    if (curl_easy_getinfo(curl, CURLINFO_EFFECTIVE_URL, &url) || !url)
        goto done;
    if (!str_caseprefix(url, "http://") && !str_caseprefix(url, "https://"))
        goto done;
    if (!(limit = memchr(buf, '\r', len)))
        goto done;
    *limit = 0;
    handle_content_disposition(buf);
    *limit = '\r';  /* Is this necessary? */
done:
    return len;
}

int main(int argc, char **argv)
{
    CURLcode res;
    char *url;

    if (argc != 2)
        die("usage");
    url = argv[1];
    curl = curl_easy_init();
    if (!curl)
        die("curl init failed");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, header_cb);
    curl_easy_setopt(curl, CURLOPT_HEADERDATA, 0);
    if ((res = curl_easy_perform(curl)) != CURLE_OK)
        die(curl_easy_strerror(res));
    curl_easy_cleanup(curl);
    printf("filename is %s\n", g_filename ? g_filename : "unknown");
    return 0;
}

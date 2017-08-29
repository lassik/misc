// Convert the given character 'chr' (Unicode codepoint) from a
// TrueType font .ttf file to a bitmap font grayscale
// run-length-encoded Targa .tga file. The image has the dimensions
// (nx * ny) as given on the command line.
//
// ./ttf2tga "/Library/Fonts/Arial Unicode.ttf" arial.tga 512 512 65

#include <stdio.h>
#include <string.h>

#include <ft2build.h>
#include FT_FREETYPE_H

static void die(const char *msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static void *xzmalloc(size_t size) {
    void *p;
    p = malloc(size);
    if(!p) die("malloc");
    memset(p, 0, size);
    return(p);
}

static void stdio_emit(FILE *stdio, void *buf, size_t size) {
    if(fwrite(buf, size, 1, stdio) != 1) die("fwrite");
}

static void emit_tga(char *tga_file, int nx, int ny, unsigned char *pixels) {
    unsigned char tga[18] = {
        0x00, 0x00, 0x0b, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x08, 0x20
    };
    unsigned char *rle;
    int nrle;
    int nnow;
    int i;
    int j;
    FILE *stdio;
    stdio = fopen(tga_file, "wb");
    if(!stdio) die("fopen");
    tga[12] = 0xff & nx;
    tga[13] = 0xff & (nx >> 8);
    tga[14] = 0xff & ny;
    tga[15] = 0xff & (ny >> 8);
    rle = xzmalloc(nx * ny);
    nrle = i = 0;
    while(i < nx * ny) {
        nnow = 1;
        for(;;) {
            if(nnow == 0x80) break;
            j = i + nnow;
            if((i / nx) != (j / nx)) break;
            if(pixels[i] != pixels[j]) break;
            ++nnow;
        }
        rle[nrle++] = 0x80 | (nnow - 1);
        rle[nrle++] = pixels[i];
        i += nnow;
    }
    stdio_emit(stdio, tga, sizeof(tga));
    stdio_emit(stdio, rle, nrle);
}

static void act(char *ttf_file, char *tga_file,
                unsigned int nx, unsigned int ny, unsigned int chr) {
    FT_Library lib;
    FT_Face face;
    unsigned char *pixels;
    unsigned int x;
    unsigned int y;
    if(FT_Init_FreeType(&lib)) die("FT_Init_FreeType");
    if(FT_New_Face(lib, ttf_file, 0, &face)) die("FT_New_Face");
    if(FT_Set_Pixel_Sizes(face, nx, ny)) die("FT_Set_Pixel_Sizes");
    if(FT_Load_Char(face, chr, FT_LOAD_RENDER)) die("FT_Load_Char");
    pixels = xzmalloc(nx * ny);
    for(x = 0; (x < nx) && (x < face->glyph->bitmap.width); ++x)
        for(y = 0; (y < ny) && (y < face->glyph->bitmap.rows); ++y)
            pixels[y * nx + x] =
                face->glyph->bitmap.buffer[y * face->glyph->bitmap.pitch + x];
    emit_tga(tga_file, nx, ny, pixels);
}

static int str_to_int(const char *str, int *out_int) {
    int n;
    n = 0;
    if(1 != sscanf(str, "%d%n", out_int, &n)) return(0);
    if((size_t)n != strlen(str)) return(0);
    return(1);
}

static void usage(void) {
    die("usage: ttf2tga <ttf-file> <tga-file> <nx> <ny> <chr>");
}

int main(int argc, char **argv) {
    int nx;
    int ny;
    int chr;
    if(argc != 6) usage();
    if(!str_to_int(argv[3], &nx)) usage();
    if(!str_to_int(argv[4], &ny)) usage();
    if(!str_to_int(argv[5], &chr)) usage();
    act(argv[1], argv[2], nx, ny, chr);
    return(0);
}

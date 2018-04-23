#include <stdio.h>
#include <string.h>

enum { width = 550, height = 400 };

int main(void) {
  static unsigned char pixels[width * height * 3];
  static unsigned char tga[18];
  unsigned char *p;
  size_t x, y;

  p = pixels;
  for (y = 0; y < height; y++) {
    float intensity = (((float)y) / height);
    for (x = 0; x < width; x++) {
      *p++ = 255 * intensity * ((float)y / height); // blue
      *p++ = 255 * intensity * ((float)x / width);  // green
      *p++ = 255 * intensity * ((float)y / height); // red
    }
  }
  tga[2] = 2;
  tga[12] = 255 & width;
  tga[13] = 255 & (width >> 8);
  tga[14] = 255 & height;
  tga[15] = 255 & (height >> 8);
  tga[16] = 24; // Number of bits in a stored pixel
  tga[17] = 32; // Origin in upper left-hand corner (y=0 x=0 top left)
  return !((1 == fwrite(tga, sizeof(tga), 1, stdout)) &&
           (1 == fwrite(pixels, sizeof(pixels), 1, stdout)));
}

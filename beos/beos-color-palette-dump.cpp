// BeOS program that dumps the color palette used for icons

// g++ -Wall -g -lbe -o beos-color-palette-dump beos-color-palette-dump.cpp

#include <Application.h>
#include <be/interface/GraphicsDefs.h>
#include <be/interface/Screen.h>
#include <stdio.h>

int main()
{
    BScreen *main_screen;
    rgb_color color;
    int i;

    new BApplication("application/x-vnd.your-app-sig");
    if(B_TRANSPARENT_8_BIT != 255) {
        fprintf(stderr, "error: B_TRANSPARENT_8_BIT != 255\n");
        return(1);
    }
    main_screen = new BScreen(B_MAIN_SCREEN_ID);
    for(i = 0; i < 255; ++i) {
        color = main_screen->ColorForIndex(i);
        printf("0x%02x%02x%02x\n", color.red, color.green, color.blue);
    }
    return(0);
}

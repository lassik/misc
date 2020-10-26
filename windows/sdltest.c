#include <SDL/SDL.h>

static SDL_Surface *scr;

int
main(void)
{
    if (SDL_Init(SDL_INIT_VIDEO) == -1) {
        exit(1);
    }
    if (!(scr = SDL_SetVideoMode(640, 480, 16, SDL_DOUBLEBUF))) {
        exit(1);
    }
    SDL_FillRect(scr, 0, 0x0000ff);
    SDL_Flip(scr);
    SDL_Delay(2000);
    return 0;
}

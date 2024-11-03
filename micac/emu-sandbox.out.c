#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

int main() {
  const int hz = 352 * 288 * 60;
  const int frame_cycles = hz / 60;
  const int width = 352;
  const int height = 288;
  const int window_scale = 2;
  const int pixel_count = width * height;
  const int bits_per_pixel = 4;
  const int pixels_size = pixel_count * bits_per_pixel;
  const int pixels_pitch = width * bits_per_pixel;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  }
  else {
    SDL_Window *window = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width * window_scale, height * window_scale, 0);
    if (!window) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    }
    else {
      SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      }
      else {
        SDL_Texture *texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
        if (!texture) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        }
        else {
          uint8_t *pixels = (uint8_t*)malloc(pixels_size * sizeof(uint8_t));
          if (pixels == 0) {
            printf("Could not allocate memory\n");
          }
          else {
            const int bar_size = 3365;
            uint8_t *pixel_ref = pixels;
            int pixel_counter = pixel_count;
            int bar_counter = bar_size;
            uint8_t alpha = 255;
            uint8_t red = 255;
            uint8_t green = 255;
            uint8_t blue = 0;
            bool running = true;
            SDL_Event event;
            while (running) {
              while (SDL_PollEvent(&event)) {
                if (event.type == SDL_QUIT) {
                  running = false;
                }
              }
              {
                {
                  int counter = frame_cycles;
                  while (counter) {
                    *pixel_ref = alpha;
                    pixel_ref += 1;
                    *pixel_ref = red;
                    pixel_ref += 1;
                    *pixel_ref = green;
                    pixel_ref += 1;
                    *pixel_ref = blue;
                    pixel_ref += 1;
                    bar_counter -= 1;
                    if (bar_counter == 0) {
                      bar_counter = bar_size;
                      red = ~red;
                      green = ~green;
                      blue = ~blue;
                    }
                    pixel_counter -= 1;
                    if (pixel_counter == 0) {
                      pixel_ref = pixels;
                      pixel_counter = pixel_count;
                    }
                    counter -= 1;
                  }
                }
                if (SDL_UpdateTexture(texture, 0, pixels, pixels_pitch) != 0) {
                  printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                }
                else {
                  if (SDL_RenderCopy(renderer, texture, 0, 0) != 0) {
                    printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                  }
                  else {
                    SDL_RenderPresent(renderer);
                  }
                }
              }
            }
            free(pixels);
          }
          SDL_DestroyTexture(texture);
        }
        SDL_DestroyRenderer(renderer);
      }
      SDL_DestroyWindow(window);
    }
    SDL_Quit();
  }
}

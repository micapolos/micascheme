#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

int main() {
  const int hz = 352 * 288 * 4 * 60;
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
            printf("malloc error\n");
          }
          else {
            uint8_t *pixel_ref = pixels;
            int pixel_counter = pixels_size;
            int frame_counter = 0;
            uint8_t pixel;
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
                    *pixel_ref = pixel;
                    pixel_ref += 1;
                    pixel += 1;
                    pixel_counter -= 1;
                    if (pixel_counter == 0) {
                      pixel_ref = pixels;
                      pixel_counter = pixels_size;
                      frame_counter += 1;
                      pixel = frame_counter << 3;
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

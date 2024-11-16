#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_2 = SDL_CreateWindow("My window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_2) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_3 = SDL_CreateRenderer(window_2, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_3) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_4 = SDL_CreateTexture(renderer_3, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_4) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_5 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_5 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            int frame_counter_6 = 0;
            bool running_7 = true;
            SDL_Event event_8;
            int sdl_mouse_x_9 = 0;
            int sdl_mouse_y_10 = 0;
            bool sdl_mouse_pressed__11 = false;
            while (running_7) {
              while (SDL_PollEvent(&event_8)) {
                if (event_8.type == SDL_QUIT) {
                  running_7 = false;
                }
              }
              {
                uint8_t *pixel_ref_12 = pixels_5;
                uint8_t value_13 = frame_counter_6 * 8;
                int counter_14 = 405504;
                while (counter_14) {
                  *pixel_ref_12 = value_13;
                  pixel_ref_12 += 1;
                  value_13 += 1;
                  counter_14 -= 1;
                }
              }
              frame_counter_6 += 1;
              if (SDL_UpdateTexture(texture_4, 0, pixels_5, 1408) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_3, texture_4, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_3);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_2);
            SDL_DestroyRenderer(renderer_3);
            SDL_DestroyTexture(texture_4);
            free(pixels_5);
          }
        }
      }
    }
  }
}


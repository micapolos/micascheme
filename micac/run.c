#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  const int width_2 = 352;
  const int height_3 = 288;
  const int window_scale_4 = 2;
  const int pixel_count_5 = width_2 * height_3;
  const int bits_per_pixel_6 = 4;
  const int pixels_size_7 = pixel_count_5 * bits_per_pixel_6;
  const int pixels_pitch_8 = width_2 * bits_per_pixel_6;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_9 = SDL_CreateWindow("My window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width_2 * window_scale_4, height_3 * window_scale_4, 0);
    if (!window_9) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_10 = SDL_CreateRenderer(window_9, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_10) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_11 = SDL_CreateTexture(renderer_10, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, width_2, height_3);
        if (!texture_11) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_12 = (uint8_t*)malloc(pixels_size_7 * sizeof(uint8_t));
          if (pixels_12 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            int frame_counter_13 = 0;
            bool running_14 = true;
            SDL_Event event_15;
            int sdl_mouse_x_16 = 0;
            int sdl_mouse_y_17 = 0;
            bool sdl_mouse_pressed__18 = false;
            while (running_14) {
              while (SDL_PollEvent(&event_15)) {
                if (event_15.type == SDL_QUIT) {
                  running_14 = false;
                }
              }
              {
                uint8_t *pixel_ref_19 = pixels_12;
                uint8_t value_20 = frame_counter_13 * 8;
                int counter_21 = pixels_size_7;
                while (counter_21) {
                  *pixel_ref_19 = value_20;
                  pixel_ref_19 += 1;
                  value_20 += 1;
                  counter_21 -= 1;
                }
              }
              frame_counter_13 += 1;
              if (SDL_UpdateTexture(texture_11, 0, pixels_12, pixels_pitch_8) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_10, texture_11, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_10);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_9);
            SDL_DestroyRenderer(renderer_10);
            SDL_DestroyTexture(texture_11);
            free(pixels_12);
          }
        }
      }
    }
  }
}


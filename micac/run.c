#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  int video_x_3 = 0;
  int video_y_4 = 0;
  int pixel_cycle_counter_5 = 0;
  uint8_t red_6 = 0;
  uint8_t green_7 = 0;
  uint8_t blue_8 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_9 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_9) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_10 = SDL_CreateRenderer(window_9, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_10) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_11 = SDL_CreateTexture(renderer_10, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_11) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_12 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_12 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_13 = pixels_12;
            int mouse_x_14 = 0;
            int mouse_y_15 = 0;
            bool mouse_pressed__16 = false;
            uint8_t previous_clock_17 = 0;
            uint8_t clock_18 = 1;
            uint16_t bar_counter_19;
            uint8_t color_20;
            bool running_21 = true;
            SDL_Event event_22;
            int sdl_mouse_x_23 = 0;
            int sdl_mouse_y_24 = 0;
            bool sdl_mouse_pressed__25 = false;
            while (running_21) {
              while (SDL_PollEvent(&event_22)) {
                if (event_22.type == SDL_QUIT) {
                  running_21 = false;
                }
              }
              int sdl_mouse_x_26;
              int sdl_mouse_y_27;
              const uint32_t sdl_mouse_state_28 = SDL_GetMouseState(&sdl_mouse_x_26, &sdl_mouse_y_27);
              mouse_x_14 = sdl_mouse_x_26 / 2;
              mouse_y_15 = sdl_mouse_y_27 / 2;
              mouse_pressed__16 = (sdl_mouse_state_28 & 1) != 0;
              int counter_29 = 559104;
              while (counter_29) {
                previous_clock_17 = clock_18;
                clock_18 = clock_18 ^ 1;
                if (previous_clock_17 != clock_18) {
                  if (clock_18 == 1) {
                    bar_counter_19 = bar_counter_19 - 1;
                    const bool bar_counter_zero__30 = bar_counter_19 == 0;
                    if (bar_counter_zero__30) {
                      bar_counter_19 = 9950;
                      color_20 = ~color_20;
                      red_6 = color_20;
                    }
                  }
                }
                if (pixel_cycle_counter_5 == 0) {
                  const bool h_video__31 = video_x_3 < 352;
                  const bool v_video__32 = video_y_4 < 288;
                  const bool video__33 = h_video__31 && v_video__32;
                  if (video__33) {
                    *pixel_ref_13 = 255;
                    pixel_ref_13 += 1;
                    *pixel_ref_13 = red_6;
                    pixel_ref_13 += 1;
                    *pixel_ref_13 = green_7;
                    pixel_ref_13 += 1;
                    *pixel_ref_13 = blue_8;
                    pixel_ref_13 += 1;
                  }
                }
                pixel_cycle_counter_5 += 1;
                if (pixel_cycle_counter_5 == 4) {
                  pixel_cycle_counter_5 = 0;
                  video_x_3 += 1;
                  if (video_x_3 == 448) {
                    video_x_3 = 0;
                    video_y_4 += 1;
                    if (video_y_4 == 312) {
                      video_y_4 = 0;
                      pixel_ref_13 = pixels_12;
                    }
                  }
                }
                counter_29 -= 1;
              }
              if (SDL_UpdateTexture(texture_11, 0, pixels_12, 1408) != 0) {
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


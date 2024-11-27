#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  int video_x_4 = 0;
  int video_y_5 = 0;
  int pixel_cycle_counter_6 = 0;
  uint8_t red_7 = 0;
  uint8_t green_8 = 0;
  uint8_t blue_9 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_10 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_10) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_11 = SDL_CreateRenderer(window_10, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_11) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_12 = SDL_CreateTexture(renderer_11, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_12) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_13 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_13 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_14 = pixels_13;
            int mouse_x_15 = 0;
            int mouse_y_16 = 0;
            bool mouse_pressed__17 = false;
            bool clock_18 = 0;
            int reset_counter_19 = 32;
            bool _reset__20 = 1;
            bool _mouse_pressed__21;
            uint8_t counter_22;
            uint8_t result_23;
            bool done__24;
            bool old_clock_3_25;
            bool running_26 = true;
            SDL_Event event_27;
            int sdl_mouse_x_28 = 0;
            int sdl_mouse_y_29 = 0;
            bool sdl_mouse_pressed__30 = false;
            while (running_26) {
              while (SDL_PollEvent(&event_27)) {
                if (event_27.type == SDL_QUIT) {
                  running_26 = false;
                }
              }
              int sdl_mouse_x_31;
              int sdl_mouse_y_32;
              const uint32_t sdl_mouse_state_33 = SDL_GetMouseState(&sdl_mouse_x_31, &sdl_mouse_y_32);
              mouse_x_15 = sdl_mouse_x_31 / 2;
              mouse_y_16 = sdl_mouse_y_32 / 2;
              mouse_pressed__17 = (sdl_mouse_state_33 & 1) != 0;
              int index_34 = 0;
              while (index_34 != 559104) {
                clock_18 = clock_18 ^ 1;
                if (reset_counter_19 == 0) {
                  _reset__20 = 0;
                } else {
                  reset_counter_19 = reset_counter_19 - 1;
                }
                _mouse_pressed__21 = mouse_pressed__17;
                if (old_clock_3_25 != clock_18) {
                  if (clock_18 == 1) {
                    if (_reset__20) {
                      counter_22 = 0;
                      done__24 = 0;
                    } else {
                      counter_22 = counter_22 + 1 & 3;
                      if (_mouse_pressed__21 && !done__24) {
                        result_23 = counter_22;
                        done__24 = 1;
                        printf("%s: %u\n", "wynik", result_23 + 1);
                      }
                    }
                  }
                }
                old_clock_3_25 = clock_18;
                if (pixel_cycle_counter_6 == 0) {
                  const bool h_video__35 = video_x_4 < 352;
                  const bool v_video__36 = video_y_5 < 288;
                  const bool video__37 = h_video__35 && v_video__36;
                  if (video__37) {
                    *pixel_ref_14 = 255;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = red_7;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = green_8;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = blue_9;
                    pixel_ref_14 += 1;
                  }
                }
                pixel_cycle_counter_6 += 1;
                if (pixel_cycle_counter_6 == 4) {
                  pixel_cycle_counter_6 = 0;
                  video_x_4 += 1;
                  if (video_x_4 == 448) {
                    video_x_4 = 0;
                    video_y_5 += 1;
                    if (video_y_5 == 312) {
                      video_y_5 = 0;
                      pixel_ref_14 = pixels_13;
                    }
                  }
                }
                index_34 += 1;
              }
              if (SDL_UpdateTexture(texture_12, 0, pixels_13, 1408) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_11, texture_12, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_11);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_10);
            SDL_DestroyRenderer(renderer_11);
            SDL_DestroyTexture(texture_12);
            free(pixels_13);
          }
        }
      }
    }
  }
}


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
            uint64_t red_counter_19;
            uint64_t green_counter_20;
            uint64_t blue_counter_21;
            uint8_t bar_red_22;
            uint8_t bar_green_23;
            uint8_t bar_blue_24;
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
              int counter_34 = 559104;
              while (counter_34) {
                clock_18 = clock_18 ^ 1;
                if (old_clock_3_25 != clock_18) {
                  if (clock_18 == 1) {
                    red_counter_19 = red_counter_19 + 1;
                    green_counter_20 = green_counter_20 + 1;
                    blue_counter_21 = blue_counter_21 + 1;
                    if (red_counter_19 > 19940) {
                      red_counter_19 = 0;
                      bar_red_22 = ~bar_red_22;
                    }
                    if (green_counter_20 > 19920) {
                      green_counter_20 = 0;
                      bar_green_23 = ~bar_green_23;
                    }
                    if (blue_counter_21 > 19900) {
                      blue_counter_21 = 0;
                      bar_blue_24 = ~bar_blue_24;
                    }
                  }
                }
                old_clock_3_25 = clock_18;
                const bool screen__35 = video_x_4 >= 48 && video_x_4 < 304 && (video_y_5 >= 48 && video_y_5 < 240);
                uint8_t red_36 = screen__35 ? 221 : bar_red_22;
                uint8_t green_37 = screen__35 ? 221 : bar_green_23;
                uint8_t blue_38 = screen__35 ? 221 : bar_blue_24;
                if (pixel_cycle_counter_6 == 0) {
                  const bool h_video__39 = video_x_4 < 352;
                  const bool v_video__40 = video_y_5 < 288;
                  const bool video__41 = h_video__39 && v_video__40;
                  if (video__41) {
                    *pixel_ref_14 = 255;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = red_36;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = green_37;
                    pixel_ref_14 += 1;
                    *pixel_ref_14 = blue_38;
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
                counter_34 -= 1;
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


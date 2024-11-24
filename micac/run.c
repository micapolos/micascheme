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
            int _video_x_19;
            int _video_y_20;
            int _mouse_x_21;
            int _mouse_y_22;
            bool _mouse_pressed__23;
            uint64_t red_counter_24;
            uint64_t green_counter_25;
            uint64_t blue_counter_26;
            uint8_t bar_red_27;
            uint8_t bar_green_28;
            uint8_t bar_blue_29;
            bool old_clock_3_30;
            bool running_31 = true;
            SDL_Event event_32;
            int sdl_mouse_x_33 = 0;
            int sdl_mouse_y_34 = 0;
            bool sdl_mouse_pressed__35 = false;
            while (running_31) {
              while (SDL_PollEvent(&event_32)) {
                if (event_32.type == SDL_QUIT) {
                  running_31 = false;
                }
              }
              int sdl_mouse_x_36;
              int sdl_mouse_y_37;
              const uint32_t sdl_mouse_state_38 = SDL_GetMouseState(&sdl_mouse_x_36, &sdl_mouse_y_37);
              mouse_x_15 = sdl_mouse_x_36 / 2;
              mouse_y_16 = sdl_mouse_y_37 / 2;
              mouse_pressed__17 = (sdl_mouse_state_38 & 1) != 0;
              int counter_39 = 559104;
              while (counter_39) {
                clock_18 = clock_18 ^ 1;
                _video_x_19 = video_x_4;
                _video_y_20 = video_y_5;
                _mouse_x_21 = mouse_x_15;
                _mouse_y_22 = mouse_y_16;
                _mouse_pressed__23 = mouse_pressed__17;
                if (old_clock_3_30 != clock_18) {
                  if (clock_18 == 1) {
                    red_counter_24 = red_counter_24 + 1;
                    green_counter_25 = green_counter_25 + 1;
                    blue_counter_26 = blue_counter_26 + 1;
                    if (red_counter_24 > 19940) {
                      red_counter_24 = 0;
                      bar_red_27 = ~bar_red_27;
                    }
                    if (green_counter_25 > 19920) {
                      green_counter_25 = 0;
                      bar_green_28 = ~bar_green_28;
                    }
                    if (blue_counter_26 > 19900) {
                      blue_counter_26 = 0;
                      bar_blue_29 = ~bar_blue_29;
                    }
                  }
                }
                old_clock_3_30 = clock_18;
                const bool bar__40 = !_mouse_pressed__23 ^ (_video_x_19 >= 48 && _video_x_19 < 304 && (_video_y_20 >= 48 && _video_y_20 < 240));
                const bool black__41 = _video_x_19 > _mouse_x_21 ^ _video_y_20 > _mouse_y_22;
                const uint8_t background_42 = black__41 ? 0 : 221;
                uint8_t video_red_43 = bar__40 ? bar_red_27 : background_42;
                uint8_t video_green_44 = bar__40 ? bar_green_28 : background_42;
                uint8_t video_blue_45 = bar__40 ? bar_blue_29 : background_42;
                red_7 = video_red_43;
                green_8 = video_green_44;
                blue_9 = video_blue_45;
                if (pixel_cycle_counter_6 == 0) {
                  const bool h_video__46 = video_x_4 < 352;
                  const bool v_video__47 = video_y_5 < 288;
                  const bool video__48 = h_video__46 && v_video__47;
                  if (video__48) {
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
                counter_39 -= 1;
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


#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  int video_x_5 = 0;
  int video_y_6 = 0;
  int pixel_cycle_counter_7 = 0;
  uint8_t red_8 = 0;
  uint8_t green_9 = 0;
  uint8_t blue_10 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_11 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_11) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_12 = SDL_CreateRenderer(window_11, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_12) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_13 = SDL_CreateTexture(renderer_12, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_13) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_14 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_14 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_15 = pixels_14;
            int mouse_x_16 = 0;
            int mouse_y_17 = 0;
            bool mouse_pressed__18 = false;
            bool clock_19 = 0;
            int _video_x_20;
            int _video_y_21;
            int _mouse_x_22;
            int _mouse_y_23;
            bool _mouse_pressed__24;
            uint32_t red_counter_25;
            uint32_t green_counter_26;
            uint32_t blue_counter_27;
            uint32_t frame_counter_28;
            uint8_t bar_red_29;
            uint8_t bar_green_30;
            uint8_t bar_blue_31;
            bool half_clock_32;
            bool old_clock_3_33;
            bool old_half_clock_4_34;
            bool running_35 = true;
            SDL_Event event_36;
            int sdl_mouse_x_37 = 0;
            int sdl_mouse_y_38 = 0;
            bool sdl_mouse_pressed__39 = false;
            while (running_35) {
              while (SDL_PollEvent(&event_36)) {
                if (event_36.type == SDL_QUIT) {
                  running_35 = false;
                }
              }
              int sdl_mouse_x_40;
              int sdl_mouse_y_41;
              const uint32_t sdl_mouse_state_42 = SDL_GetMouseState(&sdl_mouse_x_40, &sdl_mouse_y_41);
              mouse_x_16 = sdl_mouse_x_40 / 2;
              mouse_y_17 = sdl_mouse_y_41 / 2;
              mouse_pressed__18 = (sdl_mouse_state_42 & 1) != 0;
              int index_43 = 0;
              while (index_43 != 559104) {
                clock_19 = clock_19 ^ 1;
                _video_x_20 = video_x_5;
                _video_y_21 = video_y_6;
                _mouse_x_22 = mouse_x_16;
                _mouse_y_23 = mouse_y_17;
                _mouse_pressed__24 = mouse_pressed__18;
                if (old_clock_3_33 != clock_19) {
                  if (clock_19 == 1) {
                    half_clock_32 = !half_clock_32;
                  }
                }
                old_clock_3_33 = clock_19;
                if (old_half_clock_4_34 != half_clock_32) {
                  if (half_clock_32 == 1) {
                    red_counter_25 = red_counter_25 + 1;
                    green_counter_26 = green_counter_26 + 1;
                    blue_counter_27 = blue_counter_27 + 1;
                    if (red_counter_25 > 9980) {
                      red_counter_25 = 0;
                      bar_red_29 = ~bar_red_29;
                    }
                    if (green_counter_26 > 9960) {
                      green_counter_26 = 0;
                      bar_green_30 = ~bar_green_30;
                    }
                    if (blue_counter_27 > 9950) {
                      blue_counter_27 = 0;
                      bar_blue_31 = ~bar_blue_31;
                    }
                    if (_video_x_20 == 0 && _video_y_21 == 0) {
                      frame_counter_28 = frame_counter_28 + 1;
                    }
                  }
                }
                old_half_clock_4_34 = half_clock_32;
                const bool screen__44 = _video_x_20 >= 48 && _video_x_20 < 304 && (_video_y_21 >= 48 && _video_y_21 < 240);
                const bool plasma__45 = _video_x_20 > _mouse_x_22 ^ _video_y_21 < _mouse_y_23;
                const bool bar__46 = screen__44 ^ _mouse_pressed__24;
                const uint8_t plasma_red_47 = frame_counter_28 - _video_x_20;
                const uint8_t plasma_green_48 = frame_counter_28 - _video_y_21;
                const uint8_t plasma_blue_49 = frame_counter_28 + (_video_x_20 * _video_y_21 >> 6);
                const uint8_t screen_red_50 = plasma__45 ? plasma_red_47 : 221;
                const uint8_t screen_green_51 = plasma__45 ? plasma_green_48 : 221;
                const uint8_t screen_blue_52 = plasma__45 ? plasma_blue_49 : 221;
                uint8_t video_red_53 = bar__46 ? screen_red_50 : bar_red_29;
                uint8_t video_green_54 = bar__46 ? screen_green_51 : bar_green_30;
                uint8_t video_blue_55 = bar__46 ? screen_blue_52 : bar_blue_31;
                red_8 = video_red_53;
                green_9 = video_green_54;
                blue_10 = video_blue_55;
                if (pixel_cycle_counter_7 == 0) {
                  const bool h_video__56 = video_x_5 < 352;
                  const bool v_video__57 = video_y_6 < 288;
                  const bool video__58 = h_video__56 && v_video__57;
                  if (video__58) {
                    *pixel_ref_15 = 255;
                    pixel_ref_15 += 1;
                    *pixel_ref_15 = red_8;
                    pixel_ref_15 += 1;
                    *pixel_ref_15 = green_9;
                    pixel_ref_15 += 1;
                    *pixel_ref_15 = blue_10;
                    pixel_ref_15 += 1;
                  }
                }
                pixel_cycle_counter_7 += 1;
                if (pixel_cycle_counter_7 == 4) {
                  pixel_cycle_counter_7 = 0;
                  video_x_5 += 1;
                  if (video_x_5 == 448) {
                    video_x_5 = 0;
                    video_y_6 += 1;
                    if (video_y_6 == 312) {
                      video_y_6 = 0;
                      pixel_ref_15 = pixels_14;
                    }
                  }
                }
                index_43 += 1;
              }
              if (SDL_UpdateTexture(texture_13, 0, pixels_14, 1408) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_12, texture_13, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_12);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_11);
            SDL_DestroyRenderer(renderer_12);
            SDL_DestroyTexture(texture_13);
            free(pixels_14);
          }
        }
      }
    }
  }
}


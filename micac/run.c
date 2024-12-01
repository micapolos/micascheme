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
            bool _clock_19 = 0;
            int reset_counter_20 = 32;
            bool _reset__21 = 1;
            int _video_x_22;
            int _video_y_23;
            int _mouse_x_24;
            int _mouse_y_25;
            bool _mouse_pressed__26;
            uint32_t red_counter_27;
            uint32_t green_counter_28;
            uint32_t blue_counter_29;
            uint32_t frame_counter_30;
            uint8_t bar_red_31;
            uint8_t bar_green_32;
            uint8_t bar_blue_33;
            bool half_clock_34;
            uint8_t red_35;
            uint8_t green_36;
            uint8_t blue_37;
            bool old_clock_4_38;
            bool old_half_clock_3_39;
            bool running_40 = true;
            SDL_Event event_41;
            int sdl_mouse_x_42 = 0;
            int sdl_mouse_y_43 = 0;
            bool sdl_mouse_pressed__44 = false;
            while (running_40) {
              while (SDL_PollEvent(&event_41)) {
                if (event_41.type == SDL_QUIT) {
                  running_40 = false;
                }
              }
              int sdl_mouse_x_45;
              int sdl_mouse_y_46;
              const uint32_t sdl_mouse_state_47 = SDL_GetMouseState(&sdl_mouse_x_45, &sdl_mouse_y_46);
              mouse_x_16 = sdl_mouse_x_45 / 2;
              mouse_y_17 = sdl_mouse_y_46 / 2;
              mouse_pressed__18 = (sdl_mouse_state_47 & 1) != 0;
              int index_48 = 0;
              while (index_48 != 559104) {
                _clock_19 = _clock_19 ^ 1;
                if (reset_counter_20 == 0) {
                  _reset__21 = 0;
                } else {
                  reset_counter_20 = reset_counter_20 - 1;
                }
                _video_x_22 = video_x_5;
                _video_y_23 = video_y_6;
                _mouse_x_24 = mouse_x_16;
                _mouse_y_25 = mouse_y_17;
                _mouse_pressed__26 = mouse_pressed__18;
                uint8_t video_red_49 = red_35;
                uint8_t video_green_50 = green_36;
                uint8_t video_blue_51 = blue_37;
                if (old_clock_4_38 != _clock_19) {
                  if (_clock_19 == 1) {
                    half_clock_34 = !half_clock_34 & 1;
                  }
                }
                old_clock_4_38 = _clock_19;
                if (old_half_clock_3_39 != half_clock_34) {
                  if (half_clock_34 == 1) {
                    if (_reset__21 || _mouse_pressed__26) {
                      frame_counter_30 = 0;
                      bar_red_31 = 0;
                      bar_green_32 = 0;
                      bar_blue_33 = 0;
                      red_counter_27 = 0;
                      green_counter_28 = 0;
                      blue_counter_29 = 0;
                      red_35 = 0;
                      green_36 = 0;
                      blue_37 = 0;
                    } else {
                      red_counter_27 = red_counter_27 + 1 & 4294967295;
                      green_counter_28 = green_counter_28 + 1 & 4294967295;
                      blue_counter_29 = blue_counter_29 + 1 & 4294967295;
                      if (red_counter_27 > 9980) {
                        red_counter_27 = 0;
                        bar_red_31 = ~bar_red_31 & 255;
                      }
                      if (green_counter_28 > 9960) {
                        green_counter_28 = 0;
                        bar_green_32 = ~bar_green_32 & 255;
                      }
                      if (blue_counter_29 > 9950) {
                        blue_counter_29 = 0;
                        bar_blue_33 = ~bar_blue_33 & 255;
                      }
                      if ((_video_x_22 | _video_y_23) == 0) {
                        frame_counter_30 = frame_counter_30 + 1 & 4294967295;
                      }
                      const bool screen__52 = _video_x_22 >= 48 && _video_x_22 < 304 && (_video_y_23 >= 48 && _video_y_23 < 240);
                      const bool plasma__53 = _video_x_22 >= _mouse_x_24 ^ _video_y_23 < _mouse_y_25;
                      const uint8_t plasma_red_54 = frame_counter_30 - _video_x_22 & 255;
                      const uint8_t plasma_green_55 = frame_counter_30 - _video_y_23 & 255;
                      const uint8_t plasma_blue_56 = frame_counter_30 + (_video_x_22 * _video_y_23 >> 6) & 255;
                      const uint8_t screen_red_57 = plasma__53 ? plasma_red_54 : 221;
                      const uint8_t screen_green_58 = plasma__53 ? plasma_green_55 : 221;
                      const uint8_t screen_blue_59 = plasma__53 ? plasma_blue_56 : 221;
                      red_35 = screen__52 ? screen_red_57 : bar_red_31;
                      green_36 = screen__52 ? screen_green_58 : bar_green_32;
                      blue_37 = screen__52 ? screen_blue_59 : bar_blue_33;
                    }
                  }
                }
                old_half_clock_3_39 = half_clock_34;
                red_8 = video_red_49;
                green_9 = video_green_50;
                blue_10 = video_blue_51;
                if (pixel_cycle_counter_7 == 0) {
                  const bool h_video__60 = video_x_5 < 352;
                  const bool v_video__61 = video_y_6 < 288;
                  const bool video__62 = h_video__60 && v_video__61;
                  if (video__62) {
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
                index_48 += 1;
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

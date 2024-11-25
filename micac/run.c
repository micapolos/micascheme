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
            uint16_t frame_counter_27;
            uint8_t bar_red_28;
            uint8_t bar_green_29;
            uint8_t bar_blue_30;
            bool old_clock_3_31;
            bool running_32 = true;
            SDL_Event event_33;
            int sdl_mouse_x_34 = 0;
            int sdl_mouse_y_35 = 0;
            bool sdl_mouse_pressed__36 = false;
            while (running_32) {
              while (SDL_PollEvent(&event_33)) {
                if (event_33.type == SDL_QUIT) {
                  running_32 = false;
                }
              }
              int sdl_mouse_x_37;
              int sdl_mouse_y_38;
              const uint32_t sdl_mouse_state_39 = SDL_GetMouseState(&sdl_mouse_x_37, &sdl_mouse_y_38);
              mouse_x_15 = sdl_mouse_x_37 / 2;
              mouse_y_16 = sdl_mouse_y_38 / 2;
              mouse_pressed__17 = (sdl_mouse_state_39 & 1) != 0;
              int counter_40 = 559104;
              while (counter_40) {
                clock_18 = clock_18 ^ 1;
                _video_x_19 = video_x_4;
                _video_y_20 = video_y_5;
                _mouse_x_21 = mouse_x_15;
                _mouse_y_22 = mouse_y_16;
                _mouse_pressed__23 = mouse_pressed__17;
                if (old_clock_3_31 != clock_18) {
                  if (clock_18 == 1) {
                    red_counter_24 = red_counter_24 + 1;
                    green_counter_25 = green_counter_25 + 1;
                    blue_counter_26 = blue_counter_26 + 1;
                    if (red_counter_24 > 19940) {
                      red_counter_24 = 0;
                      bar_red_28 = ~bar_red_28;
                    }
                    if (green_counter_25 > 19920) {
                      green_counter_25 = 0;
                      bar_green_29 = ~bar_green_29;
                    }
                    if (blue_counter_26 > 19900) {
                      blue_counter_26 = 0;
                      bar_blue_30 = ~bar_blue_30;
                    }
                    if (_video_x_19 == 0 && _video_y_20 == 0) {
                      frame_counter_27 = frame_counter_27 + 1;
                    }
                  }
                }
                old_clock_3_31 = clock_18;
                const bool screen__41 = _video_x_19 >= 48 && _video_x_19 < 304 && (_video_y_20 >= 48 && _video_y_20 < 240);
                const bool plasma__42 = _video_x_19 > _mouse_x_21 ^ _video_y_20 < _mouse_y_22;
                const bool bar__43 = screen__41 ^ _mouse_pressed__23;
                const uint8_t counter_8_44 = frame_counter_27 >> 0;
                const uint8_t video_x_8_45 = _video_x_19 >> 0;
                const uint8_t video_y_8_46 = _video_y_20 >> 0;
                const uint32_t video_x_18_47 = 0 | _video_x_19;
                const uint32_t video_y_18_48 = 0 | _video_y_20;
                const uint8_t plasma_red_49 = counter_8_44 - video_x_8_45;
                const uint8_t plasma_green_50 = counter_8_44 - video_y_8_46;
                const uint8_t plasma_blue_51 = counter_8_44 + ((video_x_18_47 * video_y_18_48 & 262143) >> 6);
                const uint8_t screen_red_52 = plasma__42 ? plasma_red_49 : 221;
                const uint8_t screen_green_53 = plasma__42 ? plasma_green_50 : 221;
                const uint8_t screen_blue_54 = plasma__42 ? plasma_blue_51 : 221;
                uint8_t video_red_55 = bar__43 ? screen_red_52 : bar_red_28;
                uint8_t video_green_56 = bar__43 ? screen_green_53 : bar_green_29;
                uint8_t video_blue_57 = bar__43 ? screen_blue_54 : bar_blue_30;
                red_7 = video_red_55;
                green_8 = video_green_56;
                blue_9 = video_blue_57;
                if (pixel_cycle_counter_6 == 0) {
                  const bool h_video__58 = video_x_4 < 352;
                  const bool v_video__59 = video_y_5 < 288;
                  const bool video__60 = h_video__58 && v_video__59;
                  if (video__60) {
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
                counter_40 -= 1;
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


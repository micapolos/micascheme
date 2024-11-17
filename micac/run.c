#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  int video_x_2 = 0;
  int video_y_3 = 0;
  int pixel_cycle_counter_4 = 0;
  uint8_t red_5 = 0;
  uint8_t green_6 = 0;
  uint8_t blue_7 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_8 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_8) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_9 = SDL_CreateRenderer(window_8, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_9) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_10 = SDL_CreateTexture(renderer_9, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_10) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_11 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_11 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_12 = pixels_11;
            int mouse_x_13 = 0;
            int mouse_y_14 = 0;
            bool mouse_pressed__15 = false;
            int frame_counter_16 = 0;
            SDL_RWops *rw_ops_17 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
            if (!rw_ops_17) {
              printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
            } else {
              size_t scr_size_18;
              uint8_t *scr_19 = SDL_LoadFile_RW(rw_ops_17, &scr_size_18, 0);
              if (!scr_19) {
                printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
              } else {
                int bar_counter_20 = 0;
                uint8_t background_red_21 = 255;
                uint8_t background_green_22 = 255;
                uint8_t background_blue_23 = 0;
                uint8_t bits_24;
                uint8_t attr_25;
                bool ula_screen__26 = false;
                uint8_t ula_red_27 = 0;
                uint8_t ula_green_28 = 0;
                uint8_t ula_blue_29 = 0;
                uint8_t plasma_red_30;
                uint8_t plasma_green_31;
                uint8_t plasma_blue_32;
                bool running_33 = true;
                SDL_Event event_34;
                int sdl_mouse_x_35 = 0;
                int sdl_mouse_y_36 = 0;
                bool sdl_mouse_pressed__37 = false;
                while (running_33) {
                  while (SDL_PollEvent(&event_34)) {
                    if (event_34.type == SDL_QUIT) {
                      running_33 = false;
                    }
                  }
                  int sdl_mouse_x_38;
                  int sdl_mouse_y_39;
                  const uint32_t sdl_mouse_state_40 = SDL_GetMouseState(&sdl_mouse_x_38, &sdl_mouse_y_39);
                  mouse_x_13 = sdl_mouse_x_38 / 2;
                  mouse_y_14 = sdl_mouse_y_39 / 2;
                  mouse_pressed__15 = (sdl_mouse_state_40 & 1) != 0;
                  int counter_41 = 559104;
                  while (counter_41) {
                    if (pixel_cycle_counter_4 == 0) {
                      bar_counter_20 += 1;
                      if (bar_counter_20 == 4630) {
                        bar_counter_20 = 0;
                        background_red_21 = ~background_red_21;
                        background_green_22 = ~background_green_22;
                        background_blue_23 = ~background_blue_23;
                      }
                      ula_screen__26 = video_x_2 >= 48 && video_x_2 < 304 && (video_y_3 >= 48 && video_y_3 < 240);
                      if (ula_screen__26) {
                        const int ula_x_42 = video_x_2 - 48;
                        const int ula_y_43 = video_y_3 - 48;
                        const bool read__44 = (ula_x_42 & 7) == 0;
                        if (read__44) {
                          const int addr_x_45 = ula_x_42 >> 3 & 31;
                          const int bits_addr_46 = addr_x_45 | (ula_y_43 & 192 | (ula_y_43 & 7) << 3 | (ula_y_43 & 56) >> 3) << 5;
                          const int load_addr_47 = frame_counter_16 << 1;
                          const bool bits__48 = bits_addr_46 >> 3 > load_addr_47;
                          bits_24 = bits__48 ? 255 : scr_19[bits_addr_46];
                          const int attr_addr_49 = 6144 | addr_x_45 | ula_y_43 >> 3 << 5;
                          const bool attr__50 = attr_addr_49 >> 3 > load_addr_47;
                          attr_25 = attr__50 ? 7 : scr_19[attr_addr_49];
                        }
                        const bool pixel_on__51 = (bits_24 & 128) != 0;
                        bits_24 = bits_24 << 1;
                        const bool flash_on__52 = (attr_25 & 128) != 0;
                        const bool alternate_on__53 = (frame_counter_16 & 16) != 0;
                        const bool ink_on__54 = flash_on__52 && alternate_on__53 ? !pixel_on__51 : pixel_on__51;
                        const bool red__55 = (attr_25 & (ink_on__54 ? 2 : 16)) != 0;
                        const bool green__56 = (attr_25 & (ink_on__54 ? 4 : 32)) != 0;
                        const bool blue__57 = (attr_25 & (ink_on__54 ? 1 : 8)) != 0;
                        const bool bright__58 = (attr_25 & 64) != 0;
                        const uint8_t color_59 = bright__58 ? 255 : 187;
                        ula_red_27 = red__55 ? color_59 : 0;
                        ula_green_28 = green__56 ? color_59 : 0;
                        ula_blue_29 = blue__57 ? color_59 : 0;
                      }
                      plasma_red_30 = frame_counter_16 - video_x_2;
                      plasma_green_31 = frame_counter_16 - video_y_3;
                      plasma_blue_32 = frame_counter_16 + (video_x_2 * video_y_3 >> 6);
                      if (ula_screen__26) {
                        const bool plasma__60 = video_x_2 >= mouse_x_13 && video_y_3 >= mouse_y_14 || video_x_2 < mouse_x_13 && video_y_3 < mouse_y_14;
                        if (plasma__60 ^ mouse_pressed__15) {
                          red_5 = ula_red_27;
                          green_6 = ula_green_28;
                          blue_7 = ula_blue_29;
                        } else {
                          red_5 = plasma_red_30;
                          green_6 = plasma_green_31;
                          blue_7 = plasma_blue_32;
                        }
                      } else {
                        red_5 = background_red_21;
                        green_6 = background_green_22;
                        blue_7 = background_blue_23;
                      }
                      const bool frame_start__61 = video_x_2 == 0 && video_y_3 == 0;
                      if (frame_start__61) {
                        frame_counter_16 += 1;
                      }
                    }
                    if (pixel_cycle_counter_4 == 0) {
                      const bool h_video__62 = video_x_2 < 352;
                      const bool v_video__63 = video_y_3 < 288;
                      const bool video__64 = h_video__62 && v_video__63;
                      if (video__64) {
                        *pixel_ref_12 = 255;
                        pixel_ref_12 += 1;
                        *pixel_ref_12 = red_5;
                        pixel_ref_12 += 1;
                        *pixel_ref_12 = green_6;
                        pixel_ref_12 += 1;
                        *pixel_ref_12 = blue_7;
                        pixel_ref_12 += 1;
                      }
                    }
                    pixel_cycle_counter_4 += 1;
                    if (pixel_cycle_counter_4 == 4) {
                      pixel_cycle_counter_4 = 0;
                      video_x_2 += 1;
                      if (video_x_2 == 448) {
                        video_x_2 = 0;
                        video_y_3 += 1;
                        if (video_y_3 == 312) {
                          video_y_3 = 0;
                          pixel_ref_12 = pixels_11;
                        }
                      }
                    }
                    counter_41 -= 1;
                  }
                  if (SDL_UpdateTexture(texture_10, 0, pixels_11, 1408) != 0) {
                    printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                  } else {
                    if (SDL_RenderCopy(renderer_9, texture_10, 0, 0) != 0) {
                      printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                    } else {
                      SDL_RenderPresent(renderer_9);
                    }
                  }
                }
                SDL_Quit();
                SDL_DestroyWindow(window_8);
                SDL_DestroyRenderer(renderer_9);
                SDL_DestroyTexture(texture_10);
                free(pixels_11);
                SDL_RWclose(rw_ops_17);
                free(scr_19);
              }
            }
          }
        }
      }
    }
  }
}


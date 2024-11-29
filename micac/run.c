#import <stdlib.h>
#import <stdio.h>
#import <stdbool.h>
#import <SDL.h>
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
            int frame_counter_17 = 0;
            SDL_RWops *rw_ops_18 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
            if (!rw_ops_18) {
              printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
            } else {
              size_t scr_size_19;
              uint8_t *scr_20 = SDL_LoadFile_RW(rw_ops_18, &scr_size_19, 0);
              if (!scr_20) {
                printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
              } else {
                int bar_counter_21 = 0;
                uint8_t background_red_22 = 255;
                uint8_t background_green_23 = 255;
                uint8_t background_blue_24 = 0;
                uint8_t bits_25;
                uint8_t attr_26;
                bool ula_screen__27 = false;
                uint8_t ula_red_28 = 0;
                uint8_t ula_green_29 = 0;
                uint8_t ula_blue_30 = 0;
                uint8_t plasma_red_31;
                uint8_t plasma_green_32;
                uint8_t plasma_blue_33;
                bool running_34 = true;
                SDL_Event event_35;
                int sdl_mouse_x_36 = 0;
                int sdl_mouse_y_37 = 0;
                bool sdl_mouse_pressed__38 = false;
                while (running_34) {
                  while (SDL_PollEvent(&event_35)) {
                    if (event_35.type == SDL_QUIT) {
                      running_34 = false;
                    }
                  }
                  int sdl_mouse_x_39;
                  int sdl_mouse_y_40;
                  const uint32_t sdl_mouse_state_41 = SDL_GetMouseState(&sdl_mouse_x_39, &sdl_mouse_y_40);
                  mouse_x_14 = sdl_mouse_x_39 / 2;
                  mouse_y_15 = sdl_mouse_y_40 / 2;
                  mouse_pressed__16 = (sdl_mouse_state_41 & 1) != 0;
                  int index_42 = 0;
                  while (index_42 != 559104) {
                    if (pixel_cycle_counter_5 == 0) {
                      bar_counter_21 += 1;
                      if (bar_counter_21 == 4630) {
                        bar_counter_21 = 0;
                        background_red_22 = ~background_red_22;
                        background_green_23 = ~background_green_23;
                        background_blue_24 = ~background_blue_24;
                      }
                      ula_screen__27 = video_x_3 >= 48 && video_x_3 < 304 && (video_y_4 >= 48 && video_y_4 < 240);
                      if (ula_screen__27) {
                        const int ula_x_43 = video_x_3 - 48;
                        const int ula_y_44 = video_y_4 - 48;
                        const bool read__45 = (ula_x_43 & 7) == 0;
                        if (read__45) {
                          const int addr_x_46 = ula_x_43 >> 3 & 31;
                          const int bits_addr_47 = addr_x_46 | (ula_y_44 & 192 | (ula_y_44 & 7) << 3 | (ula_y_44 & 56) >> 3) << 5;
                          const int load_addr_48 = frame_counter_17 << 1;
                          const bool bits__49 = bits_addr_47 >> 3 > load_addr_48;
                          bits_25 = bits__49 ? 255 : scr_20[bits_addr_47];
                          const int attr_addr_50 = 6144 | addr_x_46 | ula_y_44 >> 3 << 5;
                          const bool attr__51 = attr_addr_50 >> 3 > load_addr_48;
                          attr_26 = attr__51 ? 7 : scr_20[attr_addr_50];
                        }
                        const bool pixel_on__52 = (bits_25 & 128) != 0;
                        bits_25 = bits_25 << 1;
                        const bool flash_on__53 = (attr_26 & 128) != 0;
                        const bool alternate_on__54 = (frame_counter_17 & 16) != 0;
                        const bool ink_on__55 = flash_on__53 && alternate_on__54 ? !pixel_on__52 : pixel_on__52;
                        const bool red__56 = (attr_26 & (ink_on__55 ? 2 : 16)) != 0;
                        const bool green__57 = (attr_26 & (ink_on__55 ? 4 : 32)) != 0;
                        const bool blue__58 = (attr_26 & (ink_on__55 ? 1 : 8)) != 0;
                        const bool bright__59 = (attr_26 & 64) != 0;
                        const uint8_t color_60 = bright__59 ? 255 : 187;
                        ula_red_28 = red__56 ? color_60 : 0;
                        ula_green_29 = green__57 ? color_60 : 0;
                        ula_blue_30 = blue__58 ? color_60 : 0;
                      }
                      plasma_red_31 = frame_counter_17 - video_x_3;
                      plasma_green_32 = frame_counter_17 - video_y_4;
                      plasma_blue_33 = frame_counter_17 + (video_x_3 * video_y_4 >> 6);
                      if (ula_screen__27) {
                        const bool plasma__61 = video_x_3 >= mouse_x_14 && video_y_4 >= mouse_y_15 || video_x_3 < mouse_x_14 && video_y_4 < mouse_y_15;
                        if (plasma__61 ^ mouse_pressed__16) {
                          red_6 = ula_red_28;
                          green_7 = ula_green_29;
                          blue_8 = ula_blue_30;
                        } else {
                          red_6 = plasma_red_31;
                          green_7 = plasma_green_32;
                          blue_8 = plasma_blue_33;
                        }
                      } else {
                        red_6 = background_red_22;
                        green_7 = background_green_23;
                        blue_8 = background_blue_24;
                      }
                      const bool frame_start__62 = video_x_3 == 0 && video_y_4 == 0;
                      if (frame_start__62) {
                        frame_counter_17 += 1;
                      }
                    }
                    if (pixel_cycle_counter_5 == 0) {
                      const bool h_video__63 = video_x_3 < 352;
                      const bool v_video__64 = video_y_4 < 288;
                      const bool video__65 = h_video__63 && v_video__64;
                      if (video__65) {
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
                    index_42 += 1;
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
                SDL_RWclose(rw_ops_18);
                free(scr_20);
              }
            }
          }
        }
      }
    }
  }
}

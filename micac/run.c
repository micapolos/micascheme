#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
int main() {
  int video_x_43 = 0;
  int video_y_44 = 0;
  int pixel_cycle_counter_45 = 0;
  uint8_t red_46 = 0;
  uint8_t green_47 = 0;
  uint8_t blue_48 = 0;
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_49 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_49) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_50 = SDL_CreateRenderer(window_49, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_50) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_51 = SDL_CreateTexture(renderer_50, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_51) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_52 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_52 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_53 = pixels_52;
            int mouse_x_54 = 0;
            int mouse_y_55 = 0;
            bool mouse_pressed__56 = false;
            int frame_counter_57 = 0;
            SDL_RWops *rw_ops_58 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
            if (!rw_ops_58) {
              printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
            } else {
              size_t scr_size_59;
              uint8_t *scr_60 = SDL_LoadFile_RW(rw_ops_58, &scr_size_59, 0);
              if (!scr_60) {
                printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
              } else {
                int bar_counter_61 = 0;
                uint8_t background_red_62 = 255;
                uint8_t background_green_63 = 255;
                uint8_t background_blue_64 = 0;
                uint8_t bits_65;
                uint8_t attr_66;
                bool ula_screen__67 = false;
                uint8_t ula_red_68 = 0;
                uint8_t ula_green_69 = 0;
                uint8_t ula_blue_70 = 0;
                uint8_t plasma_red_71;
                uint8_t plasma_green_72;
                uint8_t plasma_blue_73;
                bool running_74 = true;
                SDL_Event event_75;
                int sdl_mouse_x_76 = 0;
                int sdl_mouse_y_77 = 0;
                bool sdl_mouse_pressed__78 = false;
                while (running_74) {
                  while (SDL_PollEvent(&event_75)) {
                    if (event_75.type == SDL_QUIT) {
                      running_74 = false;
                    }
                  }
                  int sdl_mouse_x_79;
                  int sdl_mouse_y_80;
                  const uint32_t sdl_mouse_state_81 = SDL_GetMouseState(&sdl_mouse_x_79, &sdl_mouse_y_80);
                  mouse_x_54 = sdl_mouse_x_79 / 2;
                  mouse_y_55 = sdl_mouse_y_80 / 2;
                  mouse_pressed__56 = (sdl_mouse_state_81 & 1) != 0;
                  int index_82 = 0;
                  while (index_82 != 559104) {
                    if (pixel_cycle_counter_45 == 0) {
                      bar_counter_61 += 1;
                      if (bar_counter_61 == 4630) {
                        bar_counter_61 = 0;
                        background_red_62 = ~background_red_62;
                        background_green_63 = ~background_green_63;
                        background_blue_64 = ~background_blue_64;
                      }
                      ula_screen__67 = video_x_43 >= 48 && video_x_43 < 304 && (video_y_44 >= 48 && video_y_44 < 240);
                      if (ula_screen__67) {
                        const int ula_x_83 = video_x_43 - 48;
                        const int ula_y_84 = video_y_44 - 48;
                        const bool read__85 = (ula_x_83 & 7) == 0;
                        if (read__85) {
                          const int addr_x_86 = ula_x_83 >> 3 & 31;
                          const int bits_addr_87 = addr_x_86 | (ula_y_84 & 192 | (ula_y_84 & 7) << 3 | (ula_y_84 & 56) >> 3) << 5;
                          const int load_addr_88 = frame_counter_57 << 1;
                          const bool bits__89 = bits_addr_87 >> 3 > load_addr_88;
                          bits_65 = bits__89 ? 255 : scr_60[bits_addr_87];
                          const int attr_addr_90 = 6144 | addr_x_86 | ula_y_84 >> 3 << 5;
                          const bool attr__91 = attr_addr_90 >> 3 > load_addr_88;
                          attr_66 = attr__91 ? 7 : scr_60[attr_addr_90];
                        }
                        const bool pixel_on__92 = (bits_65 & 128) != 0;
                        bits_65 = bits_65 << 1;
                        const bool flash_on__93 = (attr_66 & 128) != 0;
                        const bool alternate_on__94 = (frame_counter_57 & 16) != 0;
                        const bool ink_on__95 = flash_on__93 && alternate_on__94 ? !pixel_on__92 : pixel_on__92;
                        const bool red__96 = (attr_66 & (ink_on__95 ? 2 : 16)) != 0;
                        const bool green__97 = (attr_66 & (ink_on__95 ? 4 : 32)) != 0;
                        const bool blue__98 = (attr_66 & (ink_on__95 ? 1 : 8)) != 0;
                        const bool bright__99 = (attr_66 & 64) != 0;
                        const uint8_t color_100 = bright__99 ? 255 : 187;
                        ula_red_68 = red__96 ? color_100 : 0;
                        ula_green_69 = green__97 ? color_100 : 0;
                        ula_blue_70 = blue__98 ? color_100 : 0;
                      }
                      plasma_red_71 = frame_counter_57 - video_x_43;
                      plasma_green_72 = frame_counter_57 - video_y_44;
                      plasma_blue_73 = frame_counter_57 + (video_x_43 * video_y_44 >> 6);
                      if (ula_screen__67) {
                        const bool plasma__101 = video_x_43 >= mouse_x_54 && video_y_44 >= mouse_y_55 || video_x_43 < mouse_x_54 && video_y_44 < mouse_y_55;
                        if (plasma__101 ^ mouse_pressed__56) {
                          red_46 = ula_red_68;
                          green_47 = ula_green_69;
                          blue_48 = ula_blue_70;
                        } else {
                          red_46 = plasma_red_71;
                          green_47 = plasma_green_72;
                          blue_48 = plasma_blue_73;
                        }
                      } else {
                        red_46 = background_red_62;
                        green_47 = background_green_63;
                        blue_48 = background_blue_64;
                      }
                      const bool frame_start__102 = video_x_43 == 0 && video_y_44 == 0;
                      if (frame_start__102) {
                        frame_counter_57 += 1;
                      }
                    }
                    if (pixel_cycle_counter_45 == 0) {
                      const bool h_video__103 = video_x_43 < 352;
                      const bool v_video__104 = video_y_44 < 288;
                      const bool video__105 = h_video__103 && v_video__104;
                      if (video__105) {
                        *pixel_ref_53 = 255;
                        pixel_ref_53 += 1;
                        *pixel_ref_53 = red_46;
                        pixel_ref_53 += 1;
                        *pixel_ref_53 = green_47;
                        pixel_ref_53 += 1;
                        *pixel_ref_53 = blue_48;
                        pixel_ref_53 += 1;
                      }
                    }
                    pixel_cycle_counter_45 += 1;
                    if (pixel_cycle_counter_45 == 4) {
                      pixel_cycle_counter_45 = 0;
                      video_x_43 += 1;
                      if (video_x_43 == 448) {
                        video_x_43 = 0;
                        video_y_44 += 1;
                        if (video_y_44 == 312) {
                          video_y_44 = 0;
                          pixel_ref_53 = pixels_52;
                        }
                      }
                    }
                    index_82 += 1;
                  }
                  if (SDL_UpdateTexture(texture_51, 0, pixels_52, 1408) != 0) {
                    printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                  } else {
                    if (SDL_RenderCopy(renderer_50, texture_51, 0, 0) != 0) {
                      printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                    } else {
                      SDL_RenderPresent(renderer_50);
                    }
                  }
                }
                SDL_Quit();
                SDL_DestroyWindow(window_49);
                SDL_DestroyRenderer(renderer_50);
                SDL_DestroyTexture(texture_51);
                free(pixels_52);
                SDL_RWclose(rw_ops_58);
                free(scr_60);
              }
            }
          }
        }
      }
    }
  }
}

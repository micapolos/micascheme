#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  int v9_video_x = 0;
  int v10_video_y = 0;
  int v11_pixel_cycle_counter = 0;
  uint8_t v12_red = 0;
  uint8_t v13_green = 0;
  uint8_t v14_blue = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *v15_window = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!v15_window) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *v16_renderer = SDL_CreateRenderer(v15_window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!v16_renderer) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *v17_texture = SDL_CreateTexture(v16_renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!v17_texture) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *v22_pixels = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (v22_pixels == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *v23_pixel_ref = v22_pixels;
            int v24_mouse_x = 0;
            int v25_mouse_y = 0;
            bool v26_mouse_pressed_ = false;
            int v27_frame_counter = 0;
            SDL_RWops *v28_rw_ops = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
            if (!v28_rw_ops) {
              printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
            } else {
              size_t v29_scr_size;
              uint8_t *v30_scr = SDL_LoadFile_RW(v28_rw_ops, &v29_scr_size, 0);
              if (!v30_scr) {
                printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
              } else {
                int v31_bar_counter = 0;
                uint8_t v32_background_red = 255;
                uint8_t v33_background_green = 255;
                uint8_t v34_background_blue = 0;
                uint8_t v39_bits;
                uint8_t v40_attr;
                bool v41_ula_screen_ = false;
                uint8_t v42_ula_red = 0;
                uint8_t v43_ula_green = 0;
                uint8_t v44_ula_blue = 0;
                uint8_t v46_plasma_red;
                uint8_t v47_plasma_green;
                uint8_t v48_plasma_blue;
                bool v50_running = true;
                SDL_Event v51_event;
                int v52_sdl_mouse_x = 0;
                int v53_sdl_mouse_y = 0;
                bool v54_sdl_mouse_pressed_ = false;
                while (v50_running) {
                  while (SDL_PollEvent(&v51_event)) {
                    if (v51_event.type == SDL_QUIT) {
                      v50_running = false;
                    }
                  }
                  int v55_sdl_mouse_x;
                  int v56_sdl_mouse_y;
                  const uint32_t v57_sdl_mouse_state = SDL_GetMouseState(&v55_sdl_mouse_x, &v56_sdl_mouse_y);
                  v24_mouse_x = v55_sdl_mouse_x / 2;
                  v25_mouse_y = v56_sdl_mouse_y / 2;
                  v26_mouse_pressed_ = (v57_sdl_mouse_state & 1) != 0;
                  int v58_counter = 559104;
                  while (v58_counter) {
                    if (v11_pixel_cycle_counter == 0) {
                      v31_bar_counter += 1;
                      if (v31_bar_counter == 4630) {
                        v31_bar_counter = 0;
                        v32_background_red = ~v32_background_red;
                        v33_background_green = ~v33_background_green;
                        v34_background_blue = ~v34_background_blue;
                      }
                      v41_ula_screen_ = v9_video_x >= 48 && v9_video_x < 304 && (v10_video_y >= 48 && v10_video_y < 240);
                      if (v41_ula_screen_) {
                        const int v59_ula_x = v9_video_x - 48;
                        const int v60_ula_y = v10_video_y - 48;
                        const bool v61_read_ = (v59_ula_x & 7) == 0;
                        if (v61_read_) {
                          const int v62_addr_x = v59_ula_x >> 3 & 31;
                          const int v63_bits_addr = v62_addr_x | (v60_ula_y & 192 | (v60_ula_y & 7) << 3 | (v60_ula_y & 56) >> 3) << 5;
                          const int v64_load_addr = v27_frame_counter << 1;
                          const bool v65_bits_ = v63_bits_addr >> 3 > v64_load_addr;
                          v39_bits = v65_bits_ ? 255 : v30_scr[v63_bits_addr];
                          const int v66_attr_addr = 6144 | v62_addr_x | v60_ula_y >> 3 << 5;
                          const bool v67_attr_ = v66_attr_addr >> 3 > v64_load_addr;
                          v40_attr = v67_attr_ ? 7 : v30_scr[v66_attr_addr];
                        }
                        const bool v62_pixel_on_ = (v39_bits & 128) != 0;
                        v39_bits = v39_bits << 1;
                        const bool v63_flash_on_ = (v40_attr & 128) != 0;
                        const bool v64_alternate_on_ = (v27_frame_counter & 16) != 0;
                        const bool v65_ink_on_ = v63_flash_on_ && v64_alternate_on_ ? !v62_pixel_on_ : v62_pixel_on_;
                        const bool v66_red_ = (v40_attr & (v65_ink_on_ ? 2 : 16)) != 0;
                        const bool v67_green_ = (v40_attr & (v65_ink_on_ ? 4 : 32)) != 0;
                        const bool v68_blue_ = (v40_attr & (v65_ink_on_ ? 1 : 8)) != 0;
                        const bool v69_bright_ = (v40_attr & 64) != 0;
                        const uint8_t v70_color = v69_bright_ ? 255 : 187;
                        v42_ula_red = v66_red_ ? v70_color : 0;
                        v43_ula_green = v67_green_ ? v70_color : 0;
                        v44_ula_blue = v68_blue_ ? v70_color : 0;
                      }
                      v46_plasma_red = v27_frame_counter - v9_video_x;
                      v47_plasma_green = v27_frame_counter - v10_video_y;
                      v48_plasma_blue = v27_frame_counter + (v9_video_x * v10_video_y >> 6);
                      if (v41_ula_screen_) {
                        const bool v59_plasma_ = v9_video_x >= v24_mouse_x && v10_video_y >= v25_mouse_y || v9_video_x < v24_mouse_x && v10_video_y < v25_mouse_y;
                        if (v59_plasma_ ^ v26_mouse_pressed_) {
                          v12_red = v42_ula_red;
                          v13_green = v43_ula_green;
                          v14_blue = v44_ula_blue;
                        } else {
                          v12_red = v46_plasma_red;
                          v13_green = v47_plasma_green;
                          v14_blue = v48_plasma_blue;
                        }
                      } else {
                        v12_red = v32_background_red;
                        v13_green = v33_background_green;
                        v14_blue = v34_background_blue;
                      }
                      const bool v59_frame_start_ = v9_video_x == 0 && v10_video_y == 0;
                      if (v59_frame_start_) {
                        v27_frame_counter += 1;
                      }
                    }
                    if (v11_pixel_cycle_counter == 0) {
                      const bool v59_h_video_ = v9_video_x < 352;
                      const bool v60_v_video_ = v10_video_y < 288;
                      const bool v61_video_ = v59_h_video_ && v60_v_video_;
                      if (v61_video_) {
                        *v23_pixel_ref = 255;
                        v23_pixel_ref += 1;
                        *v23_pixel_ref = v12_red;
                        v23_pixel_ref += 1;
                        *v23_pixel_ref = v13_green;
                        v23_pixel_ref += 1;
                        *v23_pixel_ref = v14_blue;
                        v23_pixel_ref += 1;
                      }
                    }
                    v11_pixel_cycle_counter += 1;
                    if (v11_pixel_cycle_counter == 4) {
                      v11_pixel_cycle_counter = 0;
                      v9_video_x += 1;
                      if (v9_video_x == 448) {
                        v9_video_x = 0;
                        v10_video_y += 1;
                        if (v10_video_y == 312) {
                          v10_video_y = 0;
                          v23_pixel_ref = v22_pixels;
                        }
                      }
                    }
                    v58_counter -= 1;
                  }
                  if (SDL_UpdateTexture(v17_texture, 0, v22_pixels, 1408) != 0) {
                    printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                  } else {
                    if (SDL_RenderCopy(v16_renderer, v17_texture, 0, 0) != 0) {
                      printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                    } else {
                      SDL_RenderPresent(v16_renderer);
                    }
                  }
                }
                SDL_Quit();
                SDL_DestroyWindow(v15_window);
                SDL_DestroyRenderer(v16_renderer);
                SDL_DestroyTexture(v17_texture);
                free(v22_pixels);
                SDL_RWclose(v28_rw_ops);
                free(v30_scr);
              }
            }
          }
        }
      }
    }
  }
}


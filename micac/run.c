#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  const int v0_width = 352;
  const int v1_height = 288;
  const int v2_h_blank = 96;
  const int v3_v_blank = 24;
  const int v4_h_size = v0_width + v2_h_blank;
  const int v5_v_size = v1_height + v3_v_blank;
  const int v6_cycles_per_pixel = 1;
  const int v7_frame_cycles = v4_h_size * v5_v_size * v6_cycles_per_pixel;
  const int v8_window_scale = 2;
  int v9_video_x = 0;
  int v10_video_y = 0;
  int v11_pixel_cycle_counter = 0;
  uint8_t v12_red = 0;
  uint8_t v13_green = 0;
  uint8_t v14_blue = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  }
  else {
    SDL_Window *v15_window = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, v0_width * v8_window_scale, v1_height * v8_window_scale, 0);
    if (!v15_window) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    }
    else {
      SDL_Renderer *v16_renderer = SDL_CreateRenderer(v15_window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!v16_renderer) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      }
      else {
        SDL_Texture *v17_texture = SDL_CreateTexture(v16_renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, v0_width, v1_height);
        if (!v17_texture) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        }
        else {
          const int v18_pixel_count = v0_width * v1_height;
          const int v19_bits_per_pixel = 4;
          const int v20_pixels_size = v18_pixel_count * v19_bits_per_pixel;
          const int v21_pixels_pitch = v0_width * v19_bits_per_pixel;
          uint8_t *v22_pixels = (uint8_t*)malloc(v20_pixels_size * sizeof(uint8_t));
          if (v22_pixels == 0) {
            printf("Could not allocate memory.\n");
          }
          else {
            uint8_t *v23_pixel_ref = v22_pixels;
            int v24_mouse_x = 0;
            int v25_mouse_y = 0;
            bool v26_mouse_pressed_ = false;
            bool v27_clk_ = true;
            uint16_t v28_z80_address = 0;
            uint8_t v29_z80_data = 0;
            bool v30_z80_memory_request_;
            bool v31_z80_read_;
            bool v32_z80_write_;
            bool v33_z80_m1_;
            bool v34_m1_request_ = true;
            bool v35_fetch_cycle_ = true;
            bool v36_fetch_cycle_request_ = true;
            bool v37_write_cycle_ = false;
            bool v38_write_cycle_request_ = true;
            uint8_t v39_clock_cycle = 0;
            uint8_t v40_clock_cycle_request = 0;
            uint16_t v41_read_write_address = 0;
            uint16_t v42_pc = 0;
            uint8_t v43_r = 0;
            bool v44_z80_wait_ = false;
            bool v47_running = true;
            SDL_Event v48_event;
            int v49_sdl_mouse_x = 0;
            int v50_sdl_mouse_y = 0;
            bool v51_sdl_mouse_pressed_ = false;
            while (v47_running) {
              while (SDL_PollEvent(&v48_event)) {
                if (v48_event.type == SDL_QUIT) {
                  v47_running = false;
                }
              }
              {
                {
                  int v52_sdl_mouse_x;
                  int v53_sdl_mouse_y;
                  const uint32_t v54_sdl_mouse_state = SDL_GetMouseState(&v52_sdl_mouse_x, &v53_sdl_mouse_y);
                  v24_mouse_x = v52_sdl_mouse_x / v8_window_scale;
                  v25_mouse_y = v53_sdl_mouse_y / v8_window_scale;
                  v26_mouse_pressed_ = !((v54_sdl_mouse_state & 1) == 0);
                }
                int v52_counter = v7_frame_cycles;
                while (v52_counter) {
                  if (v27_clk_) {
                    v39_clock_cycle = v40_clock_cycle_request;
                    v35_fetch_cycle_ = v36_fetch_cycle_request_;
                    v37_write_cycle_ = v38_write_cycle_request_;
                    v33_z80_m1_ = v34_m1_request_;
                    if (v35_fetch_cycle_) {
                      if (v39_clock_cycle == 0) {
                        v28_z80_address = v42_pc;
                        v42_pc += 1;
                      }
                      if (v39_clock_cycle == 2) {
                        v28_z80_address = v43_r & 127;
                        v43_r += 1;
                      }
                    }
                    else {
                      v28_z80_address = v41_read_write_address;
                    }
                  }
                  else {
                    const uint8_t v53_last_clock_cycle = v35_fetch_cycle_ ? 3 : 2;
                    const bool v54_last_clock_cycle_ = v39_clock_cycle == v53_last_clock_cycle;
                    v40_clock_cycle_request = v54_last_clock_cycle_ ? 0 : v39_clock_cycle + 1;
                    v30_z80_memory_request_ = !v54_last_clock_cycle_;
                    v34_m1_request_ = v36_fetch_cycle_request_ && (v39_clock_cycle & 2) == 0;
                  }
                  v27_clk_ = !v27_clk_;
                  v12_red = v33_z80_m1_ ? 255 : 0;
                  v13_green = v28_z80_address & 255;
                  if (v11_pixel_cycle_counter == 0) {
                    const bool v53_h_video_ = v9_video_x < v0_width;
                    const bool v54_v_video_ = v10_video_y < v1_height;
                    const bool v55_video_ = v53_h_video_ && v54_v_video_;
                    if (v55_video_) {
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
                  if (v11_pixel_cycle_counter == v6_cycles_per_pixel) {
                    v11_pixel_cycle_counter = 0;
                    v9_video_x += 1;
                    if (v9_video_x == v4_h_size) {
                      v9_video_x = 0;
                      v10_video_y += 1;
                      if (v10_video_y == v5_v_size) {
                        v10_video_y = 0;
                        v23_pixel_ref = v22_pixels;
                      }
                    }
                  }
                  v52_counter -= 1;
                }
                if (SDL_UpdateTexture(v17_texture, 0, v22_pixels, v21_pixels_pitch) != 0) {
                  printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                }
                else {
                  if (SDL_RenderCopy(v16_renderer, v17_texture, 0, 0) != 0) {
                    printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                  }
                  else {
                    SDL_RenderPresent(v16_renderer);
                  }
                }
              }
            }
            free(v22_pixels);
          }
          SDL_DestroyTexture(v17_texture);
        }
        SDL_DestroyRenderer(v16_renderer);
      }
      SDL_DestroyWindow(v15_window);
    }
    SDL_Quit();
  }
}


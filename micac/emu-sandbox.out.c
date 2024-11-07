#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  const int width = 352;
  const int height = 288;
  const int h_blank = 96;
  const int v_blank = 24;
  const int h_size = width + h_blank;
  const int v_size = height + v_blank;
  const int cycles_per_pixel = 4;
  const int frame_cycles = h_size * v_size * cycles_per_pixel;
  const int window_scale = 2;
  int h_counter = 0;
  int v_counter = 0;
  int pixel_cycle_counter = 0;
  uint8_t red = 0;
  uint8_t green = 0;
  uint8_t blue = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  }
  else {
    SDL_Window *window = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width * window_scale, height * window_scale, 0);
    if (!window) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    }
    else {
      SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      }
      else {
        SDL_Texture *texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, width, height);
        if (!texture) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        }
        else {
          const int pixel_count = width * height;
          const int bits_per_pixel = 4;
          const int pixels_size = pixel_count * bits_per_pixel;
          const int pixels_pitch = width * bits_per_pixel;
          uint8_t *pixels = (uint8_t*)malloc(pixels_size * sizeof(uint8_t));
          if (pixels == 0) {
            printf("Could not allocate memory.\n");
          }
          else {
            uint8_t *pixel_ref = pixels;
            const bool ula_ = true;
            const int border = 48;
            const int h_screen = 256;
            const int v_screen = 192;
            const int bar_size = 4630;
            int bar_counter = 0;
            uint8_t bg_red = 255;
            uint8_t bg_green = 255;
            uint8_t bg_blue = 0;
            int frame_counter = 0;
            uint8_t bits;
            uint8_t attr;
            SDL_RWops *rw_ops = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Robocop.scr", "rb");
            if (!rw_ops) {
              printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
            }
            else {
              size_t data_size;
              uint8_t *data = SDL_LoadFile_RW(rw_ops, &data_size, 0);
              if (!data) {
                printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
              }
              else {
                bool running = true;
                SDL_Event event;
                int sdl_mouse_x = 0;
                int sdl_mouse_y = 0;
                bool sdl_mouse_pressed_ = false;
                while (running) {
                  while (SDL_PollEvent(&event)) {
                    if (event.type == SDL_QUIT) {
                      running = false;
                    }
                  }
                  {
                    uint32_t sdl_mouse_state;
                    sdl_mouse_state = SDL_GetMouseState(&sdl_mouse_x, &sdl_mouse_y);
                    sdl_mouse_pressed_ = !((sdl_mouse_state & 1) == 0);
                    int counter = frame_cycles;
                    while (counter) {
                      if (pixel_cycle_counter == 0) {
                        const bool screen_ = h_counter >= border && h_counter < border + h_screen && (v_counter >= border && v_counter < border + v_screen);
                        if (screen_) {
                          const int x = h_counter - border;
                          const int y = v_counter - border;
                          const bool read_ = (x & 7) == 0;
                          if (read_) {
                            const int h_addr = x >> 3 & 31;
                            const int bits_addr = h_addr | (y & 192 | (y & 7) << 3 | (y & 56) >> 3) << 5;
                            const int load_addr = frame_counter << 1;
                            const bool bits_ = bits_addr >> 3 > load_addr;
                            bits = bits_ ? 255 : data[bits_addr];
                            const int attr_addr = 6144 | h_addr | y >> 3 << 5;
                            const bool attr_ = attr_addr >> 3 > load_addr;
                            attr = attr_ ? 7 : data[attr_addr];
                          }
                          const bool pixel_on_ = !((bits & 128) == 0);
                          bits = bits << 1;
                          const bool flash_on_ = !((attr & 128) == 0);
                          const bool alternate_on_ = !((frame_counter & 16) == 0);
                          const bool ink_on_ = flash_on_ && alternate_on_ ? !pixel_on_ : pixel_on_;
                          const bool red_ = !((attr & (ink_on_ ? 2 : 16)) == 0);
                          const bool green_ = !((attr & (ink_on_ ? 4 : 32)) == 0);
                          const bool blue_ = !((attr & (ink_on_ ? 1 : 8)) == 0);
                          const bool bright_ = !((attr & 64) == 0);
                          const uint8_t color = bright_ ? 255 : 187;
                          if (ula_) {
                            red = red_ ? color : 0;
                            green = green_ ? color : 0;
                            blue = blue_ ? color : 0;
                          }
                          else {
                            red = frame_counter - h_counter;
                            green = frame_counter - v_counter;
                            blue = frame_counter + (h_counter * v_counter >> 6);
                          }
                        }
                        else {
                          red = bg_red;
                          green = bg_green;
                          blue = bg_blue;
                        }
                        bar_counter += 1;
                        if (bar_counter == bar_size) {
                          bar_counter = 0;
                          bg_red = ~bg_red;
                          bg_green = ~bg_green;
                          bg_blue = ~bg_blue;
                        }
                        const bool frame_start_ = h_counter == 0 && v_counter == 0;
                        if (frame_start_) {
                          frame_counter += 1;
                        }
                      }
                      if (pixel_cycle_counter == 0) {
                        const bool h_video_ = h_counter < width;
                        const bool v_video_ = v_counter < height;
                        const bool video_ = h_video_ && v_video_;
                        if (video_) {
                          *pixel_ref = 255;
                          pixel_ref += 1;
                          *pixel_ref = red;
                          pixel_ref += 1;
                          *pixel_ref = green;
                          pixel_ref += 1;
                          *pixel_ref = blue;
                          pixel_ref += 1;
                        }
                      }
                      pixel_cycle_counter += 1;
                      if (pixel_cycle_counter == cycles_per_pixel) {
                        pixel_cycle_counter = 0;
                        h_counter += 1;
                        if (h_counter == h_size) {
                          h_counter = 0;
                          v_counter += 1;
                          if (v_counter == v_size) {
                            v_counter = 0;
                            pixel_ref = pixels;
                          }
                        }
                      }
                      counter -= 1;
                    }
                    if (SDL_UpdateTexture(texture, 0, pixels, pixels_pitch) != 0) {
                      printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                    }
                    else {
                      if (SDL_RenderCopy(renderer, texture, 0, 0) != 0) {
                        printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                      }
                      else {
                        SDL_RenderPresent(renderer);
                      }
                    }
                  }
                }
                free(data);
              }
              SDL_RWclose(rw_ops);
            }
            free(pixels);
          }
          SDL_DestroyTexture(texture);
        }
        SDL_DestroyRenderer(renderer);
      }
      SDL_DestroyWindow(window);
    }
    SDL_Quit();
  }
}


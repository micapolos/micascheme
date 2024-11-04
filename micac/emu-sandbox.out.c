#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

int main() {
  const int hz = 448 * 312 * 60;
  const int frame_cycles = hz / 60;
  const int width = 352;
  const int height = 288;
  const int window_scale = 2;
  const int pixel_count = width * height;
  const int bits_per_pixel = 4;
  const int pixels_size = pixel_count * bits_per_pixel;
  const int pixels_pitch = width * bits_per_pixel;
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
          uint8_t *pixels = (uint8_t*)malloc(pixels_size * sizeof(uint8_t));
          if (pixels == 0) {
            printf("Could not allocate memory.\n");
          }
          else {
            const int border = 48;
            const int h_screen = 256;
            const int h_blank = 96;
            const int v_screen = 192;
            const int v_blank = 24;
            const int h_size = border + h_screen + border + h_blank;
            const int v_size = border + v_screen + border + v_blank;
            uint8_t *pixel_ref = pixels;
            const int bar_size = 4630;
            int bar_counter = bar_size;
            uint8_t bg_red = 255;
            uint8_t bg_green = 255;
            uint8_t bg_blue = 0;
            int v_counter = 0;
            int h_counter = 0;
            bool h_screen_ = true;
            bool v_screen_ = true;
            bool h_pixel_ = true;
            bool v_pixel_ = true;
            bool running = true;
            SDL_Event event;
            while (running) {
              while (SDL_PollEvent(&event)) {
                if (event.type == SDL_QUIT) {
                  running = false;
                }
              }
              {
                int counter = frame_cycles;
                while (counter) {
                  {
                    uint8_t red;
                    uint8_t green;
                    uint8_t blue;
                    const bool screen_ = h_screen_ && v_screen_;
                    if (screen_) {
                      red = 192;
                      green = 192;
                      blue = 192;
                    }
                    else {
                      red = bg_red;
                      green = bg_green;
                      blue = bg_blue;
                    }
                    if (h_pixel_ && v_pixel_) {
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
                  bar_counter -= 1;
                  if (bar_counter == 0) {
                    bar_counter = bar_size;
                    bg_red = ~bg_red;
                    bg_green = ~bg_green;
                    bg_blue = ~bg_blue;
                  }
                  h_counter += 1;
                  if (h_counter == h_size) {
                    h_counter = 0;
                    v_counter += 1;
                    if (v_counter == v_size) {
                      v_counter = 0;
                    }
                  }
                  {
                    const bool h_screen_start_ = h_counter == 0;
                    const bool h_screen_end_ = h_counter == h_screen;
                    const bool v_screen_start_ = v_counter == 0;
                    const bool v_screen_end_ = v_counter == v_screen;
                    const bool h_pixel_start_ = h_counter == h_screen + border + h_blank;
                    const bool h_pixel_end_ = h_counter == h_screen + border;
                    const bool v_pixel_start_ = v_counter == v_screen + border + v_blank;
                    const bool v_pixel_end_ = v_counter == v_screen + border;
                    const bool h_screen_flip_ = h_screen_start_ || h_screen_end_;
                    const bool v_screen_flip_ = h_screen_start_ && (v_screen_start_ || v_screen_end_);
                    const bool h_pixel_flip_ = h_pixel_start_ || h_pixel_end_;
                    const bool v_pixel_flip_ = h_pixel_start_ && (v_pixel_start_ || v_pixel_end_);
                    if (h_screen_flip_) {
                      h_screen_ = !h_screen_;
                    }
                    if (v_screen_flip_) {
                      v_screen_ = !v_screen_;
                    }
                    if (h_pixel_flip_) {
                      h_pixel_ = !h_pixel_;
                    }
                    if (v_pixel_flip_) {
                      v_pixel_ = !v_pixel_;
                    }
                    const bool screen_start_ = h_screen_start_ && v_screen_start_;
                    const bool pixel_start_ = h_pixel_start_ && v_pixel_start_;
                    if (pixel_start_) {
                      pixel_ref = pixels;
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

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>

int main() {
  const int v0_width = 352;
  const int v1_height = 288;
  const int v2_window_scale = 2;
  const int v3_pixel_count = v0_width * v1_height;
  const int v4_bits_per_pixel = 4;
  const int v5_pixels_size = v3_pixel_count * v4_bits_per_pixel;
  const int v6_pixels_pitch = v0_width * v4_bits_per_pixel;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *v7_window = SDL_CreateWindow("My window", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, v0_width * v2_window_scale, v1_height * v2_window_scale, 0);
    if (!v7_window) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *v8_renderer = SDL_CreateRenderer(v7_window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!v8_renderer) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *v9_texture = SDL_CreateTexture(v8_renderer, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, v0_width, v1_height);
        if (!v9_texture) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *v10_pixels = (uint8_t*)malloc(v5_pixels_size * sizeof(uint8_t));
          if (v10_pixels == 0) {
            printf("Could not allocate memory.\n");
          } else {
            int v11_frame_counter = 0;
            bool v12_running = true;
            SDL_Event v13_event;
            int v14_sdl_mouse_x = 0;
            int v15_sdl_mouse_y = 0;
            bool v16_sdl_mouse_pressed_ = false;
            while (v12_running) {
              while (SDL_PollEvent(&v13_event)) {
                if (v13_event.type == SDL_QUIT) {
                  v12_running = false;
                }
              }
              {
                uint8_t *v17_pixel_ref = v10_pixels;
                uint8_t v18_value = v11_frame_counter * 8;
                int v19_counter = v5_pixels_size;
                while (v19_counter) {
                  *v17_pixel_ref = v18_value;
                  v17_pixel_ref += 1;
                  v18_value += 1;
                  v19_counter -= 1;
                }
              }
              v11_frame_counter += 1;
              if (SDL_UpdateTexture(v9_texture, 0, v10_pixels, v6_pixels_pitch) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(v8_renderer, v9_texture, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(v8_renderer);
                }
              }
            }
            free(v10_pixels);
          }
          SDL_DestroyTexture(v9_texture);
        }
        SDL_DestroyRenderer(v8_renderer);
      }
      SDL_DestroyWindow(v7_window);
    }
    SDL_Quit();
  }
}


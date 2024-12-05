#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
int main() {
  int video_x_140 = 0;
  int video_y_141 = 0;
  int pixel_cycle_counter_142 = 0;
  uint8_t red_143 = 0;
  uint8_t green_144 = 0;
  uint8_t blue_145 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_146 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_146) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_147 = SDL_CreateRenderer(window_146, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_147) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_148 = SDL_CreateTexture(renderer_147, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_148) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_149 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_149 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_150 = pixels_149;
            int mouse_x_151 = 0;
            int mouse_y_152 = 0;
            bool mouse_pressed__153 = false;
            bool _clock_154 = 0;
            int reset_counter_155 = 32;
            bool _reset__156 = 1;
            int _video_x_157;
            int _video_y_158;
            int _mouse_x_159;
            int _mouse_y_160;
            bool _mouse_pressed__161;
            uint32_t red_counter_162;
            uint32_t green_counter_163;
            uint32_t blue_counter_164;
            uint32_t frame_counter_165;
            uint8_t bar_red_166;
            uint8_t bar_green_167;
            uint8_t bar_blue_168;
            bool half_clock_169;
            uint8_t red_170;
            uint8_t green_171;
            uint8_t blue_172;
            bool old_clock_139_173;
            bool old_half_clock_138_174;
            bool running_175 = true;
            SDL_Event event_176;
            int sdl_mouse_x_177 = 0;
            int sdl_mouse_y_178 = 0;
            bool sdl_mouse_pressed__179 = false;
            while (running_175) {
              while (SDL_PollEvent(&event_176)) {
                if (event_176.type == SDL_QUIT) {
                  running_175 = false;
                }
              }
              int sdl_mouse_x_180;
              int sdl_mouse_y_181;
              const uint32_t sdl_mouse_state_182 = SDL_GetMouseState(&sdl_mouse_x_180, &sdl_mouse_y_181);
              mouse_x_151 = sdl_mouse_x_180 / 2;
              mouse_y_152 = sdl_mouse_y_181 / 2;
              mouse_pressed__153 = (sdl_mouse_state_182 & 1) != 0;
              int index_183 = 0;
              while (index_183 != 559104) {
                _clock_154 = _clock_154 ^ 1;
                if (reset_counter_155 == 0) {
                  _reset__156 = 0;
                } else {
                  reset_counter_155 = reset_counter_155 - 1;
                }
                _video_x_157 = video_x_140;
                _video_y_158 = video_y_141;
                _mouse_x_159 = mouse_x_151;
                _mouse_y_160 = mouse_y_152;
                _mouse_pressed__161 = mouse_pressed__153;
                uint8_t video_red_184 = red_170;
                uint8_t video_green_185 = green_171;
                uint8_t video_blue_186 = blue_172;
                if (old_clock_139_173 != _clock_154) {
                  if (_clock_154 == 1) {
                    half_clock_169 = !half_clock_169 & 1;
                  }
                }
                old_clock_139_173 = _clock_154;
                if (old_half_clock_138_174 != half_clock_169) {
                  if (half_clock_169 == 1) {
                    if (_reset__156 || _mouse_pressed__161) {
                      frame_counter_165 = 0;
                      bar_red_166 = 0;
                      bar_green_167 = 0;
                      bar_blue_168 = 0;
                      red_counter_162 = 0;
                      green_counter_163 = 0;
                      blue_counter_164 = 0;
                      red_170 = 0;
                      green_171 = 0;
                      blue_172 = 0;
                    } else {
                      red_counter_162 = red_counter_162 + 1 & 4294967295;
                      green_counter_163 = green_counter_163 + 1 & 4294967295;
                      blue_counter_164 = blue_counter_164 + 1 & 4294967295;
                      if (red_counter_162 > 9980) {
                        red_counter_162 = 0;
                        bar_red_166 = ~bar_red_166 & 255;
                      }
                      if (green_counter_163 > 9960) {
                        green_counter_163 = 0;
                        bar_green_167 = ~bar_green_167 & 255;
                      }
                      if (blue_counter_164 > 9950) {
                        blue_counter_164 = 0;
                        bar_blue_168 = ~bar_blue_168 & 255;
                      }
                      if ((_video_x_157 | _video_y_158) == 0) {
                        frame_counter_165 = frame_counter_165 + 1 & 4294967295;
                      }
                      const bool screen__187 = _video_x_157 >= 48 && _video_x_157 < 304 && (_video_y_158 >= 48 && _video_y_158 < 240);
                      const bool plasma__188 = _video_x_157 >= _mouse_x_159 ^ _video_y_158 < _mouse_y_160;
                      const uint8_t plasma_red_189 = frame_counter_165 - _video_x_157 & 255;
                      const uint8_t plasma_green_190 = frame_counter_165 - _video_y_158 & 255;
                      const uint8_t plasma_blue_191 = frame_counter_165 + (_video_x_157 * _video_y_158 >> 6) & 255;
                      const uint8_t screen_red_192 = plasma__188 ? plasma_red_189 : 221;
                      const uint8_t screen_green_193 = plasma__188 ? plasma_green_190 : 221;
                      const uint8_t screen_blue_194 = plasma__188 ? plasma_blue_191 : 221;
                      red_170 = screen__187 ? screen_red_192 : bar_red_166;
                      green_171 = screen__187 ? screen_green_193 : bar_green_167;
                      blue_172 = screen__187 ? screen_blue_194 : bar_blue_168;
                    }
                  }
                }
                old_half_clock_138_174 = half_clock_169;
                red_143 = video_red_184;
                green_144 = video_green_185;
                blue_145 = video_blue_186;
                if (pixel_cycle_counter_142 == 0) {
                  const bool h_video__195 = video_x_140 < 352;
                  const bool v_video__196 = video_y_141 < 288;
                  const bool video__197 = h_video__195 && v_video__196;
                  if (video__197) {
                    *pixel_ref_150 = 255;
                    pixel_ref_150 += 1;
                    *pixel_ref_150 = red_143;
                    pixel_ref_150 += 1;
                    *pixel_ref_150 = green_144;
                    pixel_ref_150 += 1;
                    *pixel_ref_150 = blue_145;
                    pixel_ref_150 += 1;
                  }
                }
                pixel_cycle_counter_142 += 1;
                if (pixel_cycle_counter_142 == 4) {
                  pixel_cycle_counter_142 = 0;
                  video_x_140 += 1;
                  if (video_x_140 == 448) {
                    video_x_140 = 0;
                    video_y_141 += 1;
                    if (video_y_141 == 312) {
                      video_y_141 = 0;
                      pixel_ref_150 = pixels_149;
                    }
                  }
                }
                index_183 += 1;
              }
              if (SDL_UpdateTexture(texture_148, 0, pixels_149, 1408) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_147, texture_148, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_147);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_146);
            SDL_DestroyRenderer(renderer_147);
            SDL_DestroyTexture(texture_148);
            free(pixels_149);
          }
        }
      }
    }
  }
}

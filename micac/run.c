#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
int main() {
  int video_x_139 = 0;
  int video_y_140 = 0;
  int pixel_cycle_counter_141 = 0;
  uint8_t red_142 = 0;
  uint8_t green_143 = 0;
  uint8_t blue_144 = 0;
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_145 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_145) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_146 = SDL_CreateRenderer(window_145, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_146) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_147 = SDL_CreateTexture(renderer_146, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_147) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_148 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_148 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_149 = pixels_148;
            int mouse_x_150 = 0;
            int mouse_y_151 = 0;
            bool mouse_pressed__152 = false;
            bool _clock_153 = 0;
            int reset_counter_154 = 32;
            bool _reset__155 = 1;
            int _video_x_156;
            int _video_y_157;
            int _mouse_x_158;
            int _mouse_y_159;
            bool _mouse_pressed__160;
            uint32_t red_counter_161;
            uint32_t green_counter_162;
            uint32_t blue_counter_163;
            uint32_t frame_counter_164;
            uint8_t bar_red_165;
            uint8_t bar_green_166;
            uint8_t bar_blue_167;
            bool half_clock_168;
            uint8_t red_169;
            uint8_t green_170;
            uint8_t blue_171;
            bool old_clock_138_172;
            bool old_half_clock_137_173;
            bool running_174 = true;
            SDL_Event event_175;
            int sdl_mouse_x_176 = 0;
            int sdl_mouse_y_177 = 0;
            bool sdl_mouse_pressed__178 = false;
            while (running_174) {
              while (SDL_PollEvent(&event_175)) {
                if (event_175.type == SDL_QUIT) {
                  running_174 = false;
                }
              }
              int sdl_mouse_x_179;
              int sdl_mouse_y_180;
              const uint32_t sdl_mouse_state_181 = SDL_GetMouseState(&sdl_mouse_x_179, &sdl_mouse_y_180);
              mouse_x_150 = sdl_mouse_x_179 / 2;
              mouse_y_151 = sdl_mouse_y_180 / 2;
              mouse_pressed__152 = (sdl_mouse_state_181 & 1) != 0;
              int index_182 = 0;
              while (index_182 != 559104) {
                _clock_153 = _clock_153 ^ 1;
                if (reset_counter_154 == 0) {
                  _reset__155 = 0;
                } else {
                  reset_counter_154 = reset_counter_154 - 1;
                }
                _video_x_156 = video_x_139;
                _video_y_157 = video_y_140;
                _mouse_x_158 = mouse_x_150;
                _mouse_y_159 = mouse_y_151;
                _mouse_pressed__160 = mouse_pressed__152;
                uint8_t video_red_183 = red_169;
                uint8_t video_green_184 = green_170;
                uint8_t video_blue_185 = blue_171;
                if (old_clock_138_172 != _clock_153) {
                  if (_clock_153 == 1) {
                    half_clock_168 = !half_clock_168 & 1;
                  }
                }
                old_clock_138_172 = _clock_153;
                if (old_half_clock_137_173 != half_clock_168) {
                  if (half_clock_168 == 1) {
                    if (_reset__155 || _mouse_pressed__160) {
                      frame_counter_164 = 0;
                      bar_red_165 = 0;
                      bar_green_166 = 0;
                      bar_blue_167 = 0;
                      red_counter_161 = 0;
                      green_counter_162 = 0;
                      blue_counter_163 = 0;
                      red_169 = 0;
                      green_170 = 0;
                      blue_171 = 0;
                    } else {
                      red_counter_161 = red_counter_161 + 1 & 4294967295;
                      green_counter_162 = green_counter_162 + 1 & 4294967295;
                      blue_counter_163 = blue_counter_163 + 1 & 4294967295;
                      if (red_counter_161 > 9980) {
                        red_counter_161 = 0;
                        bar_red_165 = ~bar_red_165 & 255;
                      }
                      if (green_counter_162 > 9960) {
                        green_counter_162 = 0;
                        bar_green_166 = ~bar_green_166 & 255;
                      }
                      if (blue_counter_163 > 9950) {
                        blue_counter_163 = 0;
                        bar_blue_167 = ~bar_blue_167 & 255;
                      }
                      if ((_video_x_156 | _video_y_157) == 0) {
                        frame_counter_164 = frame_counter_164 + 1 & 4294967295;
                      }
                      const bool screen__186 = _video_x_156 >= 48 && _video_x_156 < 304 && (_video_y_157 >= 48 && _video_y_157 < 240);
                      const bool plasma__187 = _video_x_156 >= _mouse_x_158 ^ _video_y_157 < _mouse_y_159;
                      const uint8_t plasma_red_188 = frame_counter_164 - _video_x_156 & 255;
                      const uint8_t plasma_green_189 = frame_counter_164 - _video_y_157 & 255;
                      const uint8_t plasma_blue_190 = frame_counter_164 + (_video_x_156 * _video_y_157 >> 6) & 255;
                      const uint8_t screen_red_191 = plasma__187 ? plasma_red_188 : 221;
                      const uint8_t screen_green_192 = plasma__187 ? plasma_green_189 : 221;
                      const uint8_t screen_blue_193 = plasma__187 ? plasma_blue_190 : 221;
                      red_169 = screen__186 ? screen_red_191 : bar_red_165;
                      green_170 = screen__186 ? screen_green_192 : bar_green_166;
                      blue_171 = screen__186 ? screen_blue_193 : bar_blue_167;
                    }
                  }
                }
                old_half_clock_137_173 = half_clock_168;
                red_142 = video_red_183;
                green_143 = video_green_184;
                blue_144 = video_blue_185;
                if (pixel_cycle_counter_141 == 0) {
                  const bool h_video__194 = video_x_139 < 352;
                  const bool v_video__195 = video_y_140 < 288;
                  const bool video__196 = h_video__194 && v_video__195;
                  if (video__196) {
                    *pixel_ref_149 = 255;
                    pixel_ref_149 += 1;
                    *pixel_ref_149 = red_142;
                    pixel_ref_149 += 1;
                    *pixel_ref_149 = green_143;
                    pixel_ref_149 += 1;
                    *pixel_ref_149 = blue_144;
                    pixel_ref_149 += 1;
                  }
                }
                pixel_cycle_counter_141 += 1;
                if (pixel_cycle_counter_141 == 4) {
                  pixel_cycle_counter_141 = 0;
                  video_x_139 += 1;
                  if (video_x_139 == 448) {
                    video_x_139 = 0;
                    video_y_140 += 1;
                    if (video_y_140 == 312) {
                      video_y_140 = 0;
                      pixel_ref_149 = pixels_148;
                    }
                  }
                }
                index_182 += 1;
              }
              if (SDL_UpdateTexture(texture_147, 0, pixels_148, 1408) != 0) {
                printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
              } else {
                if (SDL_RenderCopy(renderer_146, texture_147, 0, 0) != 0) {
                  printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                } else {
                  SDL_RenderPresent(renderer_146);
                }
              }
            }
            SDL_Quit();
            SDL_DestroyWindow(window_145);
            SDL_DestroyRenderer(renderer_146);
            SDL_DestroyTexture(texture_147);
            free(pixels_148);
          }
        }
      }
    }
  }
}

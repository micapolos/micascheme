#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
int main() {
  int video_x_141 = 0;
  int video_y_142 = 0;
  int pixel_cycle_counter_143 = 0;
  uint8_t red_144 = 0;
  uint8_t green_145 = 0;
  uint8_t blue_146 = 0;
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_147 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_147) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_148 = SDL_CreateRenderer(window_147, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_148) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_149 = SDL_CreateTexture(renderer_148, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_149) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_150 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_150 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_151 = pixels_150;
            const int audio_samples_152 = 256;
            SDL_AudioSpec audio_spec_153;
            audio_spec_153.freq = 22050;
            audio_spec_153.format = AUDIO_U8;
            audio_spec_153.channels = 2;
            audio_spec_153.samples = audio_samples_152;
            audio_spec_153.callback = 0;
            SDL_AudioDeviceID audio_device_154 = SDL_OpenAudioDevice(0, 0, &audio_spec_153, 0, 0);
            if (audio_device_154 == 0) {
              printf("%s SDL Error: %s\n", "Could not open audio device.", SDL_GetError());
            } else {
              const int sample_buffer_size_155 = 2 * audio_samples_152;
              uint8_t *sample_buffer_156 = (uint8_t*)malloc(sample_buffer_size_155 * sizeof(uint8_t));
              if (sample_buffer_156 == 0) {
                printf("Could not allocate memory.\n");
              } else {
                uint8_t *sample_buffer_ref_157 = sample_buffer_156;
                int sample_counter_158 = 0;
                const float frame_samples_159 = (float)22050 / 60;
                const float sample_cycles_160 = 559104 / frame_samples_159;
                printf("Cycles per frame: %i\n", 559104);
                printf("Samples per frame: %f\n", frame_samples_159);
                printf("Cycles per sample: %f\n", sample_cycles_160);
                int sample_cycle_counter_161 = 0;
                bool audio_req__162 = false;
                uint8_t audio_left_163 = 128;
                uint8_t audio_right_164 = 128;
                SDL_PauseAudioDevice(audio_device_154, false);
                int mouse_x_165 = 0;
                int mouse_y_166 = 0;
                bool mouse_pressed__167 = false;
                bool _clock_168 = 0;
                int reset_counter_169 = 32;
                bool _reset__170 = 1;
                int _video_x_171;
                int _video_y_172;
                int _mouse_x_173;
                int _mouse_y_174;
                bool _mouse_pressed__175;
                uint32_t red_counter_176;
                uint32_t green_counter_177;
                uint32_t blue_counter_178;
                uint32_t frame_counter_179;
                uint8_t bar_red_180;
                uint8_t bar_green_181;
                uint8_t bar_blue_182;
                bool half_clock_183;
                uint8_t red_184;
                uint8_t green_185;
                uint8_t blue_186;
                bool old_clock_140_187;
                bool old_half_clock_139_188;
                bool running_189 = true;
                SDL_Event event_190;
                int sdl_mouse_x_191 = 0;
                int sdl_mouse_y_192 = 0;
                bool sdl_mouse_pressed__193 = false;
                while (running_189) {
                  while (SDL_PollEvent(&event_190)) {
                    if (event_190.type == SDL_QUIT) {
                      running_189 = false;
                    }
                  }
                  int sdl_mouse_x_194;
                  int sdl_mouse_y_195;
                  const uint32_t sdl_mouse_state_196 = SDL_GetMouseState(&sdl_mouse_x_194, &sdl_mouse_y_195);
                  mouse_x_165 = sdl_mouse_x_194 / 2;
                  mouse_y_166 = sdl_mouse_y_195 / 2;
                  mouse_pressed__167 = (sdl_mouse_state_196 & 1) != 0;
                  int index_197 = 0;
                  while (index_197 != 559104) {
                    audio_req__162 = sample_cycle_counter_161 == 0;
                    _clock_168 = _clock_168 ^ 1;
                    if (reset_counter_169 == 0) {
                      _reset__170 = 0;
                    } else {
                      reset_counter_169 = reset_counter_169 - 1;
                    }
                    _video_x_171 = video_x_141;
                    _video_y_172 = video_y_142;
                    _mouse_x_173 = mouse_x_165;
                    _mouse_y_174 = mouse_y_166;
                    _mouse_pressed__175 = mouse_pressed__167;
                    uint8_t video_red_198 = red_184;
                    uint8_t video_green_199 = green_185;
                    uint8_t video_blue_200 = blue_186;
                    if (old_clock_140_187 != _clock_168) {
                      if (_clock_168 == 1) {
                        half_clock_183 = !half_clock_183 & 1;
                      }
                    }
                    old_clock_140_187 = _clock_168;
                    if (old_half_clock_139_188 != half_clock_183) {
                      if (half_clock_183 == 1) {
                        if (_reset__170 || _mouse_pressed__175) {
                          frame_counter_179 = 0;
                          bar_red_180 = 0;
                          bar_green_181 = 0;
                          bar_blue_182 = 0;
                          red_counter_176 = 0;
                          green_counter_177 = 0;
                          blue_counter_178 = 0;
                          red_184 = 0;
                          green_185 = 0;
                          blue_186 = 0;
                        } else {
                          red_counter_176 = red_counter_176 + 1 & 4294967295;
                          green_counter_177 = green_counter_177 + 1 & 4294967295;
                          blue_counter_178 = blue_counter_178 + 1 & 4294967295;
                          if (red_counter_176 > 9980) {
                            red_counter_176 = 0;
                            bar_red_180 = ~bar_red_180 & 255;
                          }
                          if (green_counter_177 > 9960) {
                            green_counter_177 = 0;
                            bar_green_181 = ~bar_green_181 & 255;
                          }
                          if (blue_counter_178 > 9950) {
                            blue_counter_178 = 0;
                            bar_blue_182 = ~bar_blue_182 & 255;
                          }
                          if ((_video_x_171 | _video_y_172) == 0) {
                            frame_counter_179 = frame_counter_179 + 1 & 4294967295;
                          }
                          const bool screen__201 = _video_x_171 >= 48 && _video_x_171 < 304 && (_video_y_172 >= 48 && _video_y_172 < 240);
                          const bool plasma__202 = _video_x_171 >= _mouse_x_173 ^ _video_y_172 < _mouse_y_174;
                          const uint8_t plasma_red_203 = frame_counter_179 - _video_x_171 & 255;
                          const uint8_t plasma_green_204 = frame_counter_179 - _video_y_172 & 255;
                          const uint8_t plasma_blue_205 = frame_counter_179 + (_video_x_171 * _video_y_172 >> 6) & 255;
                          const uint8_t screen_red_206 = plasma__202 ? plasma_red_203 : 221;
                          const uint8_t screen_green_207 = plasma__202 ? plasma_green_204 : 221;
                          const uint8_t screen_blue_208 = plasma__202 ? plasma_blue_205 : 221;
                          red_184 = screen__201 ? screen_red_206 : bar_red_180;
                          green_185 = screen__201 ? screen_green_207 : bar_green_181;
                          blue_186 = screen__201 ? screen_blue_208 : bar_blue_182;
                        }
                      }
                    }
                    old_half_clock_139_188 = half_clock_183;
                    red_144 = video_red_198;
                    green_145 = video_green_199;
                    blue_146 = video_blue_200;
                    if (pixel_cycle_counter_143 == 0) {
                      const bool h_video__209 = video_x_141 < 352;
                      const bool v_video__210 = video_y_142 < 288;
                      const bool video__211 = h_video__209 && v_video__210;
                      if (video__211) {
                        *pixel_ref_151 = 255;
                        pixel_ref_151 += 1;
                        *pixel_ref_151 = red_144;
                        pixel_ref_151 += 1;
                        *pixel_ref_151 = green_145;
                        pixel_ref_151 += 1;
                        *pixel_ref_151 = blue_146;
                        pixel_ref_151 += 1;
                      }
                    }
                    if (pixel_cycle_counter_143 == 0) {
                      pixel_cycle_counter_143 = 4;
                      video_x_141 += 1;
                      if (video_x_141 == 448) {
                        video_x_141 = 0;
                        video_y_142 += 1;
                        if (video_y_142 == 312) {
                          video_y_142 = 0;
                          pixel_ref_151 = pixels_150;
                        }
                      }
                    }
                    pixel_cycle_counter_143 -= 1;
                    if (audio_req__162) {
                      sample_cycle_counter_161 += sample_cycles_160;
                      *sample_buffer_ref_157 = audio_left_163;
                      sample_buffer_ref_157 += 1;
                      *sample_buffer_ref_157 = audio_right_164;
                      sample_buffer_ref_157 += 1;
                      if (sample_counter_158 == 0) {
                        sample_counter_158 = audio_samples_152;
                        sample_buffer_ref_157 = sample_buffer_156;
                        const int queued_audio_size_212 = SDL_GetQueuedAudioSize(audio_device_154);
                        int queue_audio_count_213 = 1;
                        if (queued_audio_size_212 == 0) {
                          queue_audio_count_213 = 2;
                          printf("Audio queue underflow.\n");
                        } else if (queued_audio_size_212 >= 4 * sample_buffer_size_155) {
                          queue_audio_count_213 = 0;
                          printf("Audio queue overflow.\n");
                        }
                        int index_214 = 0;
                        while (index_214 != queue_audio_count_213) {
                          SDL_QueueAudio(audio_device_154, sample_buffer_156, sample_buffer_size_155);
                          index_214 += 1;
                        }
                      }
                      sample_counter_158 -= 1;
                    }
                    sample_cycle_counter_161 -= 1;
                    index_197 += 1;
                  }
                  if (SDL_UpdateTexture(texture_149, 0, pixels_150, 1408) != 0) {
                    printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                  } else {
                    if (SDL_RenderCopy(renderer_148, texture_149, 0, 0) != 0) {
                      printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                    } else {
                      SDL_RenderPresent(renderer_148);
                    }
                  }
                }
                SDL_Quit();
                SDL_DestroyWindow(window_147);
                SDL_DestroyRenderer(renderer_148);
                SDL_DestroyTexture(texture_149);
                free(pixels_150);
                SDL_CloseAudioDevice(audio_device_154);
                free(sample_buffer_156);
              }
            }
          }
        }
      }
    }
  }
}

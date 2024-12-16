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
            const int audio_samples_54 = 256;
            SDL_AudioSpec audio_spec_55;
            audio_spec_55.freq = 22050;
            audio_spec_55.format = AUDIO_U8;
            audio_spec_55.channels = 2;
            audio_spec_55.samples = audio_samples_54;
            audio_spec_55.callback = 0;
            SDL_AudioDeviceID audio_device_56 = SDL_OpenAudioDevice(0, 0, &audio_spec_55, 0, 0);
            if (audio_device_56 == 0) {
              printf("%s SDL Error: %s\n", "Could not open audio device.", SDL_GetError());
            } else {
              const int sample_buffer_size_57 = 2 * audio_samples_54;
              uint8_t *sample_buffer_58 = (uint8_t*)malloc(sample_buffer_size_57 * sizeof(uint8_t));
              if (sample_buffer_58 == 0) {
                printf("Could not allocate memory.\n");
              } else {
                uint8_t *sample_buffer_ref_59 = sample_buffer_58;
                int sample_counter_60 = 0;
                const float frame_samples_61 = (float)22050 / 60;
                const float sample_cycles_62 = 559104 / frame_samples_61;
                printf("Cycles per frame: %i\n", 559104);
                printf("Samples per frame: %f\n", frame_samples_61);
                printf("Cycles per sample: %f\n", sample_cycles_62);
                int sample_cycle_counter_63 = 0;
                uint8_t audio_left_64 = 128;
                uint8_t audio_right_65 = 128;
                SDL_PauseAudioDevice(audio_device_56, false);
                int mouse_x_66 = 0;
                int mouse_y_67 = 0;
                bool mouse_pressed__68 = false;
                int frame_counter_69 = 0;
                SDL_RWops *rw_ops_70 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
                if (!rw_ops_70) {
                  printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                } else {
                  size_t scr_size_71;
                  uint8_t *scr_72 = SDL_LoadFile_RW(rw_ops_70, &scr_size_71, 0);
                  if (!scr_72) {
                    printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                  } else {
                    int bar_counter_73 = 0;
                    uint8_t background_red_74 = 255;
                    uint8_t background_green_75 = 255;
                    uint8_t background_blue_76 = 0;
                    uint8_t bits_77;
                    uint8_t attr_78;
                    bool ula_screen__79 = false;
                    uint8_t ula_red_80 = 0;
                    uint8_t ula_green_81 = 0;
                    uint8_t ula_blue_82 = 0;
                    uint8_t plasma_red_83;
                    uint8_t plasma_green_84;
                    uint8_t plasma_blue_85;
                    bool running_86 = true;
                    SDL_Event event_87;
                    int sdl_mouse_x_88 = 0;
                    int sdl_mouse_y_89 = 0;
                    bool sdl_mouse_pressed__90 = false;
                    while (running_86) {
                      while (SDL_PollEvent(&event_87)) {
                        if (event_87.type == SDL_QUIT) {
                          running_86 = false;
                        }
                      }
                      int sdl_mouse_x_91;
                      int sdl_mouse_y_92;
                      const uint32_t sdl_mouse_state_93 = SDL_GetMouseState(&sdl_mouse_x_91, &sdl_mouse_y_92);
                      mouse_x_66 = sdl_mouse_x_91 / 2;
                      mouse_y_67 = sdl_mouse_y_92 / 2;
                      mouse_pressed__68 = (sdl_mouse_state_93 & 1) != 0;
                      int index_94 = 0;
                      while (index_94 != 559104) {
                        if (pixel_cycle_counter_45 == 0) {
                          bar_counter_73 += 1;
                          if (bar_counter_73 == 4630) {
                            bar_counter_73 = 0;
                            background_red_74 = ~background_red_74;
                            background_green_75 = ~background_green_75;
                            background_blue_76 = ~background_blue_76;
                          }
                          ula_screen__79 = video_x_43 >= 48 && video_x_43 < 304 && (video_y_44 >= 48 && video_y_44 < 240);
                          if (ula_screen__79) {
                            const int ula_x_95 = video_x_43 - 48;
                            const int ula_y_96 = video_y_44 - 48;
                            const bool read__97 = (ula_x_95 & 7) == 0;
                            if (read__97) {
                              const int addr_x_98 = ula_x_95 >> 3 & 31;
                              const int bits_addr_99 = addr_x_98 | (ula_y_96 & 192 | (ula_y_96 & 7) << 3 | (ula_y_96 & 56) >> 3) << 5;
                              const int load_addr_100 = frame_counter_69 << 1;
                              const bool bits__101 = bits_addr_99 >> 3 > load_addr_100;
                              bits_77 = bits__101 ? 255 : scr_72[bits_addr_99];
                              const int attr_addr_102 = 6144 | addr_x_98 | ula_y_96 >> 3 << 5;
                              const bool attr__103 = attr_addr_102 >> 3 > load_addr_100;
                              attr_78 = attr__103 ? 7 : scr_72[attr_addr_102];
                            }
                            const bool pixel_on__104 = (bits_77 & 128) != 0;
                            bits_77 = bits_77 << 1;
                            const bool flash_on__105 = (attr_78 & 128) != 0;
                            const bool alternate_on__106 = (frame_counter_69 & 16) != 0;
                            const bool ink_on__107 = flash_on__105 && alternate_on__106 ? !pixel_on__104 : pixel_on__104;
                            const bool red__108 = (attr_78 & (ink_on__107 ? 2 : 16)) != 0;
                            const bool green__109 = (attr_78 & (ink_on__107 ? 4 : 32)) != 0;
                            const bool blue__110 = (attr_78 & (ink_on__107 ? 1 : 8)) != 0;
                            const bool bright__111 = (attr_78 & 64) != 0;
                            const uint8_t color_112 = bright__111 ? 255 : 187;
                            ula_red_80 = red__108 ? color_112 : 0;
                            ula_green_81 = green__109 ? color_112 : 0;
                            ula_blue_82 = blue__110 ? color_112 : 0;
                          }
                          plasma_red_83 = frame_counter_69 - video_x_43;
                          plasma_green_84 = frame_counter_69 - video_y_44;
                          plasma_blue_85 = frame_counter_69 + (video_x_43 * video_y_44 >> 6);
                          if (ula_screen__79) {
                            const bool plasma__113 = video_x_43 >= mouse_x_66 && video_y_44 >= mouse_y_67 || video_x_43 < mouse_x_66 && video_y_44 < mouse_y_67;
                            if (plasma__113 ^ mouse_pressed__68) {
                              red_46 = ula_red_80;
                              green_47 = ula_green_81;
                              blue_48 = ula_blue_82;
                            } else {
                              red_46 = plasma_red_83;
                              green_47 = plasma_green_84;
                              blue_48 = plasma_blue_85;
                            }
                          } else {
                            red_46 = background_red_74;
                            green_47 = background_green_75;
                            blue_48 = background_blue_76;
                          }
                          audio_left_64 = plasma_green_84;
                          audio_right_65 = plasma_green_84;
                          const bool frame_start__114 = video_x_43 == 0 && video_y_44 == 0;
                          if (frame_start__114) {
                            frame_counter_69 += 1;
                          }
                        }
                        if (pixel_cycle_counter_45 == 0) {
                          const bool h_video__115 = video_x_43 < 352;
                          const bool v_video__116 = video_y_44 < 288;
                          const bool video__117 = h_video__115 && v_video__116;
                          if (video__117) {
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
                        sample_cycle_counter_63 += 1;
                        if (sample_cycle_counter_63 >= sample_cycles_62) {
                          sample_cycle_counter_63 -= sample_cycles_62;
                          *sample_buffer_ref_59 = audio_left_64;
                          sample_buffer_ref_59 += 1;
                          *sample_buffer_ref_59 = audio_right_65;
                          sample_buffer_ref_59 += 1;
                          sample_counter_60 += 1;
                          if (sample_counter_60 == audio_samples_54) {
                            sample_counter_60 = 0;
                            sample_buffer_ref_59 = sample_buffer_58;
                            SDL_QueueAudio(audio_device_56, sample_buffer_58, sample_buffer_size_57);
                          }
                        }
                        index_94 += 1;
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
                    SDL_CloseAudioDevice(audio_device_56);
                    free(sample_buffer_58);
                    SDL_RWclose(rw_ops_70);
                    free(scr_72);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

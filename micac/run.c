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
                bool audio_req__64 = false;
                uint8_t audio_left_65 = 128;
                uint8_t audio_right_66 = 128;
                SDL_PauseAudioDevice(audio_device_56, false);
                int mouse_x_67 = 0;
                int mouse_y_68 = 0;
                bool mouse_pressed__69 = false;
                int frame_counter_70 = 0;
                SDL_RWops *rw_ops_71 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
                if (!rw_ops_71) {
                  printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                } else {
                  size_t scr_size_72;
                  uint8_t *scr_73 = SDL_LoadFile_RW(rw_ops_71, &scr_size_72, 0);
                  if (!scr_73) {
                    printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                  } else {
                    int bar_counter_74 = 0;
                    uint8_t background_red_75 = 255;
                    uint8_t background_green_76 = 255;
                    uint8_t background_blue_77 = 0;
                    uint8_t bits_78;
                    uint8_t attr_79;
                    bool ula_screen__80 = false;
                    uint8_t ula_red_81 = 0;
                    uint8_t ula_green_82 = 0;
                    uint8_t ula_blue_83 = 0;
                    uint8_t plasma_red_84;
                    uint8_t plasma_green_85;
                    uint8_t plasma_blue_86;
                    bool running_87 = true;
                    SDL_Event event_88;
                    int sdl_mouse_x_89 = 0;
                    int sdl_mouse_y_90 = 0;
                    bool sdl_mouse_pressed__91 = false;
                    while (running_87) {
                      while (SDL_PollEvent(&event_88)) {
                        if (event_88.type == SDL_QUIT) {
                          running_87 = false;
                        }
                      }
                      int sdl_mouse_x_92;
                      int sdl_mouse_y_93;
                      const uint32_t sdl_mouse_state_94 = SDL_GetMouseState(&sdl_mouse_x_92, &sdl_mouse_y_93);
                      mouse_x_67 = sdl_mouse_x_92 / 2;
                      mouse_y_68 = sdl_mouse_y_93 / 2;
                      mouse_pressed__69 = (sdl_mouse_state_94 & 1) != 0;
                      int index_95 = 0;
                      while (index_95 != 559104) {
                        audio_req__64 = sample_cycle_counter_63 == 0;
                        if (pixel_cycle_counter_45 == 0) {
                          bar_counter_74 += 1;
                          if (bar_counter_74 == 4630) {
                            bar_counter_74 = 0;
                            background_red_75 = ~background_red_75;
                            background_green_76 = ~background_green_76;
                            background_blue_77 = ~background_blue_77;
                          }
                          ula_screen__80 = video_x_43 >= 48 && video_x_43 < 304 && (video_y_44 >= 48 && video_y_44 < 240);
                          if (ula_screen__80) {
                            const int ula_x_96 = video_x_43 - 48;
                            const int ula_y_97 = video_y_44 - 48;
                            const bool read__98 = (ula_x_96 & 7) == 0;
                            if (read__98) {
                              const int addr_x_99 = ula_x_96 >> 3 & 31;
                              const int bits_addr_100 = addr_x_99 | (ula_y_97 & 192 | (ula_y_97 & 7) << 3 | (ula_y_97 & 56) >> 3) << 5;
                              const int load_addr_101 = frame_counter_70 << 1;
                              const bool bits__102 = bits_addr_100 >> 3 > load_addr_101;
                              bits_78 = bits__102 ? 255 : scr_73[bits_addr_100];
                              const int attr_addr_103 = 6144 | addr_x_99 | ula_y_97 >> 3 << 5;
                              const bool attr__104 = attr_addr_103 >> 3 > load_addr_101;
                              attr_79 = attr__104 ? 7 : scr_73[attr_addr_103];
                            }
                            const bool pixel_on__105 = (bits_78 & 128) != 0;
                            bits_78 = bits_78 << 1;
                            const bool flash_on__106 = (attr_79 & 128) != 0;
                            const bool alternate_on__107 = (frame_counter_70 & 16) != 0;
                            const bool ink_on__108 = flash_on__106 && alternate_on__107 ? !pixel_on__105 : pixel_on__105;
                            const bool red__109 = (attr_79 & (ink_on__108 ? 2 : 16)) != 0;
                            const bool green__110 = (attr_79 & (ink_on__108 ? 4 : 32)) != 0;
                            const bool blue__111 = (attr_79 & (ink_on__108 ? 1 : 8)) != 0;
                            const bool bright__112 = (attr_79 & 64) != 0;
                            const uint8_t color_113 = bright__112 ? 255 : 187;
                            ula_red_81 = red__109 ? color_113 : 0;
                            ula_green_82 = green__110 ? color_113 : 0;
                            ula_blue_83 = blue__111 ? color_113 : 0;
                          }
                          plasma_red_84 = frame_counter_70 - video_x_43;
                          plasma_green_85 = frame_counter_70 - video_y_44;
                          plasma_blue_86 = frame_counter_70 + (video_x_43 * video_y_44 >> 6);
                          if (ula_screen__80) {
                            const bool plasma__114 = video_x_43 >= mouse_x_67 && video_y_44 >= mouse_y_68 || video_x_43 < mouse_x_67 && video_y_44 < mouse_y_68;
                            if (plasma__114 ^ mouse_pressed__69) {
                              red_46 = ula_red_81;
                              green_47 = ula_green_82;
                              blue_48 = ula_blue_83;
                            } else {
                              red_46 = plasma_red_84;
                              green_47 = plasma_green_85;
                              blue_48 = plasma_blue_86;
                            }
                          } else {
                            red_46 = background_red_75;
                            green_47 = background_green_76;
                            blue_48 = background_blue_77;
                          }
                          if (audio_req__64) {
                            const uint8_t audio_115 = (plasma_green_85 >> 2) + 96;
                            audio_left_65 = audio_115;
                            audio_right_66 = audio_115;
                          }
                          const bool frame_start__116 = video_x_43 == 0 && video_y_44 == 0;
                          if (frame_start__116) {
                            frame_counter_70 += 1;
                          }
                        }
                        if (pixel_cycle_counter_45 == 0) {
                          const bool h_video__117 = video_x_43 < 352;
                          const bool v_video__118 = video_y_44 < 288;
                          const bool video__119 = h_video__117 && v_video__118;
                          if (video__119) {
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
                        if (pixel_cycle_counter_45 == 0) {
                          pixel_cycle_counter_45 = 4;
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
                        pixel_cycle_counter_45 -= 1;
                        if (audio_req__64) {
                          sample_cycle_counter_63 += sample_cycles_62;
                          *sample_buffer_ref_59 = audio_left_65;
                          sample_buffer_ref_59 += 1;
                          *sample_buffer_ref_59 = audio_right_66;
                          sample_buffer_ref_59 += 1;
                          if (sample_counter_60 == 0) {
                            sample_counter_60 = audio_samples_54;
                            sample_buffer_ref_59 = sample_buffer_58;
                            const int queued_audio_size_120 = SDL_GetQueuedAudioSize(audio_device_56);
                            int queue_audio_count_121 = 1;
                            if (queued_audio_size_120 == 0) {
                              queue_audio_count_121 = 2;
                              printf("Audio queue underflow.\n");
                            } else if (queued_audio_size_120 >= 4 * sample_buffer_size_57) {
                              queue_audio_count_121 = 0;
                              printf("Audio queue overflow.\n");
                            }
                            int index_122 = 0;
                            while (index_122 != queue_audio_count_121) {
                              SDL_QueueAudio(audio_device_56, sample_buffer_58, sample_buffer_size_57);
                              index_122 += 1;
                            }
                          }
                          sample_counter_60 -= 1;
                        }
                        sample_cycle_counter_63 -= 1;
                        index_95 += 1;
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
                    SDL_RWclose(rw_ops_71);
                    free(scr_73);
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

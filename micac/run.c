#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <SDL.h>
int main() {
  int video_x_15 = 0;
  int video_y_16 = 0;
  int pixel_cycle_counter_17 = 0;
  uint8_t red_18 = 0;
  uint8_t green_19 = 0;
  uint8_t blue_20 = 0;
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0) {
    printf("%s SDL Error: %s\n", "Could not initialize.", SDL_GetError());
  } else {
    SDL_Window *window_21 = SDL_CreateWindow("Emu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 704, 576, 0);
    if (!window_21) {
      printf("%s SDL Error: %s\n", "Could not create window.", SDL_GetError());
    } else {
      SDL_Renderer *renderer_22 = SDL_CreateRenderer(window_21, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
      if (!renderer_22) {
        printf("%s SDL Error: %s\n", "Could not create renderer.", SDL_GetError());
      } else {
        SDL_Texture *texture_23 = SDL_CreateTexture(renderer_22, SDL_PIXELFORMAT_BGRA8888, SDL_TEXTUREACCESS_STREAMING, 352, 288);
        if (!texture_23) {
          printf("%s SDL Error: %s\n", "Could not create texture.", SDL_GetError());
        } else {
          uint8_t *pixels_24 = (uint8_t*)malloc(405504 * sizeof(uint8_t));
          if (pixels_24 == 0) {
            printf("Could not allocate memory.\n");
          } else {
            uint8_t *pixel_ref_25 = pixels_24;
            const int audio_samples_26 = 256;
            SDL_AudioSpec audio_spec_27;
            audio_spec_27.freq = 22050;
            audio_spec_27.format = AUDIO_U8;
            audio_spec_27.channels = 2;
            audio_spec_27.samples = audio_samples_26;
            audio_spec_27.callback = 0;
            SDL_AudioDeviceID audio_device_28 = SDL_OpenAudioDevice(0, 0, &audio_spec_27, 0, 0);
            if (audio_device_28 == 0) {
              printf("%s SDL Error: %s\n", "Could not open audio device.", SDL_GetError());
            } else {
              const int sample_buffer_size_29 = 2 * audio_samples_26;
              uint8_t *sample_buffer_30 = (uint8_t*)malloc(sample_buffer_size_29 * sizeof(uint8_t));
              if (sample_buffer_30 == 0) {
                printf("Could not allocate memory.\n");
              } else {
                uint8_t *sample_buffer_ref_31 = sample_buffer_30;
                int sample_counter_32 = 0;
                const float frame_samples_33 = (float)22050 / 60;
                const float sample_cycles_34 = 559104 / frame_samples_33;
                printf("Cycles per frame: %i\n", 559104);
                printf("Samples per frame: %f\n", frame_samples_33);
                printf("Cycles per sample: %f\n", sample_cycles_34);
                int sample_cycle_counter_35 = 0;
                bool audio_req__36 = false;
                uint8_t audio_left_37 = 128;
                uint8_t audio_right_38 = 128;
                SDL_PauseAudioDevice(audio_device_28, false);
                int mouse_x_39 = 0;
                int mouse_y_40 = 0;
                bool mouse_pressed__41 = false;
                int frame_counter_42 = 0;
                SDL_RWops *rw_ops_43 = SDL_RWFromFile("/Users/micapolos/git/micascheme/micac/scr/Cobra.scr", "rb");
                if (!rw_ops_43) {
                  printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                } else {
                  size_t scr_size_44;
                  uint8_t *scr_45 = SDL_LoadFile_RW(rw_ops_43, &scr_size_44, 0);
                  if (!scr_45) {
                    printf("%s SDL Error: %s\n", "Could not open file.", SDL_GetError());
                  } else {
                    int bar_counter_46 = 0;
                    uint8_t background_red_47 = 255;
                    uint8_t background_green_48 = 255;
                    uint8_t background_blue_49 = 0;
                    uint8_t bits_50;
                    uint8_t attr_51;
                    bool ula_screen__52 = false;
                    uint8_t ula_red_53 = 0;
                    uint8_t ula_green_54 = 0;
                    uint8_t ula_blue_55 = 0;
                    uint8_t plasma_red_56;
                    uint8_t plasma_green_57;
                    uint8_t plasma_blue_58;
                    bool running_59 = true;
                    SDL_Event event_60;
                    int sdl_mouse_x_61 = 0;
                    int sdl_mouse_y_62 = 0;
                    bool sdl_mouse_pressed__63 = false;
                    while (running_59) {
                      while (SDL_PollEvent(&event_60)) {
                        if (event_60.type == SDL_QUIT) {
                          running_59 = false;
                        }
                      }
                      int sdl_mouse_x_64;
                      int sdl_mouse_y_65;
                      const uint32_t sdl_mouse_state_66 = SDL_GetMouseState(&sdl_mouse_x_64, &sdl_mouse_y_65);
                      mouse_x_39 = sdl_mouse_x_64 / 2;
                      mouse_y_40 = sdl_mouse_y_65 / 2;
                      mouse_pressed__41 = (sdl_mouse_state_66 & 1) != 0;
                      int index_67 = 0;
                      while (index_67 != 559104) {
                        audio_req__36 = sample_cycle_counter_35 == 0;
                        if (pixel_cycle_counter_17 == 0) {
                          bar_counter_46 += 1;
                          if (bar_counter_46 == 4630) {
                            bar_counter_46 = 0;
                            background_red_47 = ~background_red_47;
                            background_green_48 = ~background_green_48;
                            background_blue_49 = ~background_blue_49;
                          }
                          ula_screen__52 = video_x_15 >= 48 && video_x_15 < 304 && (video_y_16 >= 48 && video_y_16 < 240);
                          if (ula_screen__52) {
                            const int ula_x_68 = video_x_15 - 48;
                            const int ula_y_69 = video_y_16 - 48;
                            const bool read__70 = (ula_x_68 & 7) == 0;
                            if (read__70) {
                              const int addr_x_71 = ula_x_68 >> 3 & 31;
                              const int bits_addr_72 = addr_x_71 | (ula_y_69 & 192 | (ula_y_69 & 7) << 3 | (ula_y_69 & 56) >> 3) << 5;
                              const int load_addr_73 = frame_counter_42 << 1;
                              const bool bits__74 = bits_addr_72 >> 3 > load_addr_73;
                              bits_50 = bits__74 ? 255 : scr_45[bits_addr_72];
                              const int attr_addr_75 = 6144 | addr_x_71 | ula_y_69 >> 3 << 5;
                              const bool attr__76 = attr_addr_75 >> 3 > load_addr_73;
                              attr_51 = attr__76 ? 7 : scr_45[attr_addr_75];
                            }
                            const bool pixel_on__77 = (bits_50 & 128) != 0;
                            bits_50 = bits_50 << 1;
                            const bool flash_on__78 = (attr_51 & 128) != 0;
                            const bool alternate_on__79 = (frame_counter_42 & 16) != 0;
                            const bool ink_on__80 = flash_on__78 && alternate_on__79 ? !pixel_on__77 : pixel_on__77;
                            const bool red__81 = (attr_51 & (ink_on__80 ? 2 : 16)) != 0;
                            const bool green__82 = (attr_51 & (ink_on__80 ? 4 : 32)) != 0;
                            const bool blue__83 = (attr_51 & (ink_on__80 ? 1 : 8)) != 0;
                            const bool bright__84 = (attr_51 & 64) != 0;
                            const uint8_t color_85 = bright__84 ? 255 : 187;
                            ula_red_53 = red__81 ? color_85 : 0;
                            ula_green_54 = green__82 ? color_85 : 0;
                            ula_blue_55 = blue__83 ? color_85 : 0;
                          }
                          plasma_red_56 = frame_counter_42 - video_x_15;
                          plasma_green_57 = frame_counter_42 - video_y_16;
                          plasma_blue_58 = frame_counter_42 + (video_x_15 * video_y_16 >> 6);
                          if (ula_screen__52) {
                            const bool plasma__86 = video_x_15 >= mouse_x_39 && video_y_16 >= mouse_y_40 || video_x_15 < mouse_x_39 && video_y_16 < mouse_y_40;
                            if (plasma__86 ^ mouse_pressed__41) {
                              red_18 = ula_red_53;
                              green_19 = ula_green_54;
                              blue_20 = ula_blue_55;
                            } else {
                              red_18 = plasma_red_56;
                              green_19 = plasma_green_57;
                              blue_20 = plasma_blue_58;
                            }
                          } else {
                            red_18 = background_red_47;
                            green_19 = background_green_48;
                            blue_20 = background_blue_49;
                          }
                          if (audio_req__36) {
                            const uint8_t audio_87 = (plasma_green_57 >> 2) + 96;
                            audio_left_37 = audio_87;
                            audio_right_38 = audio_87;
                          }
                          const bool frame_start__88 = video_x_15 == 0 && video_y_16 == 0;
                          if (frame_start__88) {
                            frame_counter_42 += 1;
                          }
                        }
                        if (pixel_cycle_counter_17 == 0) {
                          const bool h_video__89 = video_x_15 < 352;
                          const bool v_video__90 = video_y_16 < 288;
                          const bool video__91 = h_video__89 && v_video__90;
                          if (video__91) {
                            *pixel_ref_25 = 255;
                            pixel_ref_25 += 1;
                            *pixel_ref_25 = red_18;
                            pixel_ref_25 += 1;
                            *pixel_ref_25 = green_19;
                            pixel_ref_25 += 1;
                            *pixel_ref_25 = blue_20;
                            pixel_ref_25 += 1;
                          }
                        }
                        if (pixel_cycle_counter_17 == 0) {
                          pixel_cycle_counter_17 = 4;
                          video_x_15 += 1;
                          if (video_x_15 == 448) {
                            video_x_15 = 0;
                            video_y_16 += 1;
                            if (video_y_16 == 312) {
                              video_y_16 = 0;
                              pixel_ref_25 = pixels_24;
                            }
                          }
                        }
                        pixel_cycle_counter_17 -= 1;
                        if (audio_req__36) {
                          sample_cycle_counter_35 += sample_cycles_34;
                          *sample_buffer_ref_31 = audio_left_37;
                          sample_buffer_ref_31 += 1;
                          *sample_buffer_ref_31 = audio_right_38;
                          sample_buffer_ref_31 += 1;
                          if (sample_counter_32 == 0) {
                            sample_counter_32 = audio_samples_26;
                            sample_buffer_ref_31 = sample_buffer_30;
                            const int queued_audio_size_92 = SDL_GetQueuedAudioSize(audio_device_28);
                            int queue_audio_count_93 = 1;
                            if (queued_audio_size_92 == 0) {
                              queue_audio_count_93 = 2;
                              printf("Audio queue underflow.\n");
                            } else if (queued_audio_size_92 >= 4 * sample_buffer_size_29) {
                              queue_audio_count_93 = 0;
                              printf("Audio queue overflow.\n");
                            }
                            int index_94 = 0;
                            while (index_94 != queue_audio_count_93) {
                              SDL_QueueAudio(audio_device_28, sample_buffer_30, sample_buffer_size_29);
                              index_94 += 1;
                            }
                          }
                          sample_counter_32 -= 1;
                        }
                        sample_cycle_counter_35 -= 1;
                        index_67 += 1;
                      }
                      if (SDL_UpdateTexture(texture_23, 0, pixels_24, 1408) != 0) {
                        printf("%s SDL Error: %s\n", "Could not update texture.", SDL_GetError());
                      } else {
                        if (SDL_RenderCopy(renderer_22, texture_23, 0, 0) != 0) {
                          printf("%s SDL Error: %s\n", "Could not render copy.", SDL_GetError());
                        } else {
                          SDL_RenderPresent(renderer_22);
                        }
                      }
                    }
                    SDL_Quit();
                    SDL_DestroyWindow(window_21);
                    SDL_DestroyRenderer(renderer_22);
                    SDL_DestroyTexture(texture_23);
                    free(pixels_24);
                    SDL_CloseAudioDevice(audio_device_28);
                    free(sample_buffer_30);
                    SDL_RWclose(rw_ops_43);
                    free(scr_45);
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

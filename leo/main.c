#include <stdio.h>
#include "scheme.h"

// The xxd-generated headers from your build/ directory
#include "petite_boot.h"
#include "scheme_boot.h"
#include "leo_load_so.h"

int main(int argc, char *argv[]) {
    fprintf(stderr, "Startint LeoC...\n");

    // 1. Immediate failure if no argument is provided
    if (argc < 2) {
        fprintf(stderr, "error: missing .leo file path in main.c\n");
        return 1;
    }

    // 1. Initialize the engine
    Sscheme_init(NULL);

    // 2. Register boot files from memory
    // These must be registered BEFORE Sbuild_heap
    Sregister_boot_file_bytes("petite.boot", (void *)petite_boot, petite_boot_len);
    Sregister_boot_file_bytes("scheme.boot", (void *)scheme_boot, scheme_boot_len);
    Sregister_boot_file_bytes("leo-load.so", (void *)leo_load_so, leo_load_so_len);

    // 3. Build the heap
    Sbuild_heap(NULL, NULL);

    // 4. Start
    Scall1(Stop_level_value(Sstring_to_symbol("load-leo")), Sstring(argv[1]));

    // 5. Cleanup
    Sscheme_deinit();

    return 0;
}

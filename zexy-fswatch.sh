fswatch -o -r zexy-dot.ss | while read f; do echo "Compiling..."; scheme --script zexy-dot.ss; done

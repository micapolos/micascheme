build_leo() {
  echo "$(date +'%H:%M:%S') - Running Leo..."
  leo "$1" > "$2"
}

build_leo $1 $2

stdbuf -oL fswatch -o -r "$1" | while read -r f; do
  build_leo "$1" "$2"
done

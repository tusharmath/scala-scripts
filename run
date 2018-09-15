echo "Watching: $1"
fswatch -0 -Ie ".*" -i "$1" . | xargs -0 -n 1 -I {} scala {}

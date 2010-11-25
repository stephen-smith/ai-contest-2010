tee debug.in | { "$@" 2>&1 1>&3 | tee debug.err; } 3>&1 1>&2 | tee debug.out

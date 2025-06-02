#!/bin/bash

declare -a files=()

ret=0

function read_lines {

  while read -r line || [[ -n "$line" ]]; do
    err=$?
    if [ "$err" != 0 ]; then
      echo "ERROR while reading a file" >&2
      exit 2
    elif [[ "$line" = "PATH "* ]]; then
      path="${line#PATH }"
      if [ -L "$path" ]; then
        final_path=$(readlink "$path")
        echo "LINK '$path' '$final_path'"
      elif [ -d "$path" ]; then
        echo "DIR '$path'"
      elif [ -f "$path" ]; then
        files+=("$path")
        num_of_lines=$(wc -l < "$path")
        first_line=$(head -n 1 "$path")
        err=$?
        if [ "$err" != 0 ]; then
          echo "ERROR: permission denied" >&2
          exit 2
        fi
        echo "FILE '$path' $num_of_lines '$first_line'"
      else
        echo "ERROR '$path'" >&2
        ret=1
      fi
    fi
  done
  
}

if [ "$#" = 0 ]; then
  read_lines
fi

while getopts ":zh" opt; do
  case $opt in
    z)
      read_lines
      output="output.tgz"
      if ! tar -czf "$output" "${files[@]}"; then
        echo "ERROR while creating an archive" >&2
        ret=2
      fi
      ;;
    h)
      echo -e "HINT:\nScript reads lines from input. If a line starts with 'PATH ' then the script determines what is on the end of the path and prints it.\nIt accepts two switches (-z, -h). First is for creating an archive with found files, second is for this hint."
      exit 0
      ;;
    ?)
      echo "Wrong switch: -${OPTARG}" >&2
      exit 2
      ;;
  esac
done
shift $((OPTIND - 1))
if [ "$#" -gt 0 ]; then
    echo "ERROR: Wrong parameters: $*"
    exit 2
fi

exit "$ret"

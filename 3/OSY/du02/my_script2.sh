#!/bin/bash

print_hint() {
    echo "Usage: $0 [Switch] [Parameter], where [Parameter] is used only for -d switch."
    echo "-h prints this hint."
    echo "-a prints all PDF files from current directory in alphabetical order."
    echo "-b prints lines beginning with a number without this number."
    echo "-c prints all sentences beginning with Capital letter and ending with '. or ! or ?'."
    echo "-d <prefix> adds prefix to all occurences of #include."
}

case "$1" in
    -h)
        print_hint
        exit 0
        ;;
    -a)
        find . -maxdepth 1 -type f -iname "*.pdf" | sed 's^./^^' | sort
        ;;
    -b)
        sed -nE 's/^([-+]?[0-9]+)(.*)/\2/p'
        ;;
    -c)
        tr '\n' ' ' | grep -o -E '[A-ZÁČĎÉĚÍŇÓŘŠŤÚŮÝŽ][^!?.A-ZÁČĎÉĚÍŇÓŘŠŤÚŮÝŽ]*[.!?]'
        ;;
    -d)
        if [ -z "$2" ]; then
            echo "ERROR: No second argument for -d switch" >&2
            exit 1
        fi
        sed -E "s|(#[[:space:]]*include[[:space:]]*[\"])([^\"]*)([\"])|\1$2\2\3|g; s|(#[[:space:]]*include[[:space:]]*[<])([^<>]*)([>])|\1$2\2\3|g"
        ;;
    *)
        echo "ERROR: Wrong switch: $1" >&2
        print_hint
        exit 1
        ;;
esac

exit 0

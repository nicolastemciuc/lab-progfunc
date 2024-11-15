#!/bin/zsh

# Create necessary directories
mkdir -p ./casos/misalida
mkdir -p ./casos/diferencias

# Process cases for lint and suggestions
for i in {01..24}; do
  input_file="./casos/caso${i}.mhs"
  output_file="./casos/misalida/caso${i}-lint"
  diff_output="./casos/diferencias/diferencias_caso${i}-lint.txt"

  if [[ -f "$input_file" ]]; then
    echo "Procesando archivo caso${i}-${suffix}"
    ./Linter -c "$input_file" > "$output_file"
    diff -w "./casos/salidas/caso${i}-lint.mhs" "$output_file" > "$diff_output"
  else
    echo "File $input_file not found!"
  fi
done

# Process cases for lint and suggestions
for i in {01..24}; do
  input_file="./casos/caso${i}.mhs"
  output_file="./casos/misalida/caso${i}-sug"
  diff_output="./casos/diferencias/diferencias_caso${i}-sug.txt"

  if [[ -f "$input_file" ]]; then
    echo "Procesando archivo caso${i}-${suffix}"
    ./Linter -s "$input_file" > "$output_file"
    diff -w "./casos/salidas/caso${i}-sug" "$output_file" > "$diff_output"
  else
    echo "File $input_file not found!"
  fi
done

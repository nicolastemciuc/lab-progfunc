#!/bin/zsh

ghc -o Linter Linter.hs
rm -rf ./casos/misalida
rm -rf ./casos/diferencias

# Crear directorios necesarios
mkdir -p ./casos/misalida
mkdir -p ./casos/diferencias

echo "--------------Resultado--------------"
for i in {01..24}; do
  input_file="./casos/caso${i}.mhs"
  output_file="./casos/misalida/caso${i}-lint"
  diff_output="./casos/diferencias/diferencias_caso${i}-lint.txt"

  if [[ -f "$input_file" ]]; then
    ./Linter -c "$input_file" > "$output_file"
    diff -w "./casos/salidas/caso${i}-lint.mhs" "$output_file" > "$diff_output"
    if [[ -s "$diff_output" ]]; then
      echo "❌${i}"
    else
      echo "✅${i}"
      rm "$diff_output" # Eliminar archivo de diferencias si está vacío
    fi
  else
    echo "File $input_file not found!"
  fi
done

echo "--------------Suggestions--------------"
# Procesar casos para suggestions
for i in {01..24}; do
  input_file="./casos/caso${i}.mhs"
  output_file="./casos/misalida/caso${i}-sug"
  diff_output="./casos/diferencias/diferencias_caso${i}-sug.txt"

  if [[ -f "$input_file" ]]; then
    ./Linter -s "$input_file" > "$output_file"
    diff -w "./casos/salidas/caso${i}-sug" "$output_file" > "$diff_output"
    if [[ -s "$diff_output" ]]; then
      echo "❌${i}"
    else
      echo "✅${i}"
      rm "$diff_output" # Eliminar archivo de diferencias si está vacío
    fi
  else
    echo "File $input_file not found!"
  fi
done
echo "--------------Fin--------------"

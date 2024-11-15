@echo off
setlocal enabledelayedexpansion

:: Compile the Haskell program
ghc -o Linter Linter.hs

:: Remove directories if they exist
rmdir /S /Q .\casos\misalida 2>nul
rmdir /S /Q .\casos\diferencias 2>nul

:: Create necessary directories
mkdir .\casos\misalida
mkdir .\casos\diferencias

echo --------------Resultado--------------
for /l %%i in (1,1,24) do (
    set "num=%%i"
    if %%i LSS 10 set "num=0%%i"

    set "input_file=.\casos\caso!num!.mhs"
    set "output_file=.\casos\misalida\caso!num!-lint"
    set "diff_output=.\casos\diferencias\diferencias_caso!num!-lint.txt"
    set "expected_output=.\casos\salidas\caso!num!-lint.mhs"

    if exist "!input_file!" (
        Linter.exe -c "!input_file!" > "!output_file!"
        if exist "!expected_output!" (
            fc /w "!expected_output!" "!output_file!" > nul
            if errorlevel 1 (
                echo ❌!num!
                fc /w "!expected_output!" "!output_file!" > "!diff_output!"
            ) else (
                echo ✅!num!
                del "!diff_output!" 2>nul
            )
        ) else (
            echo Expected output file "!expected_output!" not found!
        )
    ) else (
        echo File "!input_file!" not found!
    )
)

echo --------------Suggestions--------------
for /l %%i in (1,1,24) do (
    set "num=%%i"
    if %%i LSS 10 set "num=0%%i"

    set "input_file=.\casos\caso!num!.mhs"
    set "output_file=.\casos\misalida\caso!num!-sug"
    set "diff_output=.\casos\diferencias\diferencias_caso!num!-sug.txt"
    set "expected_output=.\casos\salidas\caso!num!-sug"

    if exist "!input_file!" (
        Linter.exe -s "!input_file!" > "!output_file!"
        if exist "!expected_output!" (
            fc /w "!expected_output!" "!output_file!" > nul
            if errorlevel 1 (
                echo ❌!num!
                fc /w "!expected_output!" "!output_file!" > "!diff_output!"
            ) else (
                echo ✅!num!
                del "!diff_output!" 2>nul
            )
        ) else (
            echo Expected output file "!expected_output!" not found!
        )
    ) else (
        echo File "!input_file!" not found!
    )
)

echo --------------Fin--------------

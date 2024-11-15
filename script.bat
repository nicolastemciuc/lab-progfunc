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

    if exist "!input_file!" (
        Linter.exe -c "!input_file!" > "!output_file!"
        fc /w .\casos\salidas\caso!num!-lint.mhs "!output_file!" > "!diff_output!" 2>nul
        if exist "!diff_output!" (
            for /f %%A in ('find /c /v "" "!diff_output!"') do set "lines=%%A"
            if !lines! GTR 0 (
                echo ❌!num!
            ) else (
                echo ✅!num!
                del "!diff_output!"
            )
        ) else (
            echo ✅!num!
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

    if exist "!input_file!" (
        Linter.exe -s "!input_file!" > "!output_file!"
        fc /w .\casos\salidas\caso!num!-sug "!output_file!" > "!diff_output!" 2>nul
        if exist "!diff_output!" (
            for /f %%A in ('find /c /v "" "!diff_output!"') do set "lines=%%A"
            if !lines! GTR 0 (
                echo ❌!num!
            ) else (
                echo ✅!num!
                del "!diff_output!"
            )
        ) else (
            echo ✅!num!
        )
    ) else (
        echo File "!input_file!" not found!
    )
)

echo --------------Fin--------------

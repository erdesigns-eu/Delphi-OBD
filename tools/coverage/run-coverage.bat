@echo off
REM ---------------------------------------------------------------
REM run-coverage.bat — invokes delphi-code-coverage against the
REM compiled Tests.exe. Produces an HTML report in tools/coverage/out
REM and a Cobertura XML report suitable for codecov upload.
REM
REM Prerequisites:
REM   1. Delphi 11+ installed.
REM   2. delphi-code-coverage on PATH (https://github.com/DelphiCodeCoverage/DelphiCodeCoverage).
REM   3. Tests.exe + Tests.map already built — run
REM        msbuild Packages\RunTime.dproj  /p:Config=Debug
REM        msbuild tests\Tests.dproj       /p:Config=Debug
REM      first.
REM   4. Sources compiled with detailed map info: dcc32 -GD or
REM      project option 'Detailed' under Linker > Map file.
REM
REM Usage:
REM   tools\coverage\run-coverage.bat path\to\Tests.exe
REM ---------------------------------------------------------------

setlocal enabledelayedexpansion

if "%~1"=="" (
  set "TEST_EXE=tests\Win32\Debug\Tests.exe"
) else (
  set "TEST_EXE=%~1"
)

if not exist "%TEST_EXE%" (
  echo [ERROR] Tests.exe not found at %TEST_EXE%
  echo Build the test project first, e.g. msbuild tests\Tests.dproj /p:Config=Debug
  exit /b 1
)

set "TEST_MAP=%TEST_EXE:.exe=.map%"
if not exist "%TEST_MAP%" (
  echo [ERROR] %TEST_MAP% not found.
  echo Make sure the linker is configured to emit a Detailed map file.
  exit /b 1
)

set "OUT_DIR=tools\coverage\out"
if not exist "%OUT_DIR%" mkdir "%OUT_DIR%"

REM Source-path list — every directory we want delphi-code-coverage
REM to scan when looking up included units. Add new src subfolders
REM here as they appear.
set SRC_PATHS=src\Services;src\Protocol;src\Utilities;src\Connection

echo Running delphi-code-coverage against %TEST_EXE%
delphi-code-coverage ^
  -e "%TEST_EXE%" ^
  -m "%TEST_MAP%" ^
  -uf tools\coverage\cov-include.txt ^
  -spa "%SRC_PATHS%" ^
  -od "%OUT_DIR%" ^
  -html ^
  -xml ^
  -emma21 ^
  -lcov

if errorlevel 1 (
  echo [ERROR] coverage run failed
  exit /b 1
)

echo.
echo Coverage report written to %OUT_DIR%\
echo  - CodeCoverage_summary.html  (browser)
echo  - CodeCoverage_summary.xml   (Cobertura, for codecov)
echo  - coverage.lcov              (lcov, for CI dashboards)
endlocal

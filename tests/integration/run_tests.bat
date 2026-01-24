@echo off
REM run_tests.bat - Run AdaModbus integration tests on Windows
REM Copyright (c) 2026 Florian Fischer
REM SPDX-License-Identifier: MIT
REM
REM This script:
REM 1. Starts the Modbus simulator in the background
REM 2. Builds the Ada integration tests
REM 3. Runs the tests
REM 4. Stops the simulator
REM
REM Usage: run_tests.bat [port]
REM   Default port: 5020

setlocal enabledelayedexpansion

set PORT=5020
if not "%1"=="" set PORT=%1

set SCRIPT_DIR=%~dp0
set PROJECT_ROOT=%SCRIPT_DIR%..\..

echo ========================================
echo   AdaModbus Integration Test Runner
echo ========================================
echo.

REM Check for Python
where python >nul 2>&1
if errorlevel 1 (
    echo Error: Python not found
    echo Please install Python 3.8 or later
    exit /b 1
)

REM Check for pymodbus
python -c "import pymodbus" 2>nul
if errorlevel 1 (
    echo Warning: pymodbus not found
    echo Installing pymodbus...
    pip install pymodbus
)

REM Check for alr
where alr >nul 2>&1
if errorlevel 1 (
    echo Error: Alire ^(alr^) not found
    echo Please install Alire from https://alire.ada.dev
    exit /b 1
)

REM Start simulator in background
echo Starting Modbus simulator on port %PORT%...
cd /d "%SCRIPT_DIR%"
start /b python modbus_simulator.py --port %PORT%

REM Wait for simulator to start
timeout /t 3 /nobreak >nul

echo Simulator started
echo.

REM Build tests
echo Building integration tests...
cd /d "%PROJECT_ROOT%"
alr build -- -P tests/integration/integration_tests.gpr

if errorlevel 1 (
    echo Build failed
    goto :cleanup
)

echo Build successful
echo.

REM Run tests
echo Running integration tests...
echo ----------------------------------------
"%PROJECT_ROOT%\bin\integration_test_runner.exe"
set TEST_RESULT=%errorlevel%
echo ----------------------------------------

if %TEST_RESULT%==0 (
    echo.
    echo All tests passed!
) else (
    echo.
    echo Some tests failed
)

:cleanup
REM Stop simulator
echo.
echo Stopping simulator...
taskkill /f /im python.exe /fi "WINDOWTITLE eq modbus_simulator*" >nul 2>&1

REM Alternative: kill all Python processes running modbus_simulator
REM This is more aggressive but ensures cleanup
for /f "tokens=2" %%a in ('tasklist /fi "imagename eq python.exe" /fo list ^| findstr "PID:"') do (
    wmic process where "ProcessId=%%a" get CommandLine 2>nul | findstr /i "modbus_simulator" >nul
    if not errorlevel 1 (
        taskkill /f /pid %%a >nul 2>&1
    )
)

exit /b %TEST_RESULT%

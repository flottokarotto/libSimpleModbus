#!/bin/bash
#
# run_tests.sh - Run AdaModbus integration tests
# Copyright (c) 2026 Florian Fischer
# SPDX-License-Identifier: MIT
#
# This script:
# 1. Starts the Modbus simulator in the background
# 2. Builds the Ada integration tests
# 3. Runs the tests
# 4. Stops the simulator
#
# Usage: ./run_tests.sh [--port PORT] [--verbose]

set -e

# Configuration
PORT=${PORT:-5020}
VERBOSE=""
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --port)
            PORT="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE="--verbose"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--port PORT] [--verbose]"
            echo ""
            echo "Options:"
            echo "  --port PORT    TCP port for simulator (default: 5020)"
            echo "  --verbose, -v  Enable verbose output"
            echo "  --help, -h     Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "  AdaModbus Integration Test Runner"
echo "========================================"
echo ""

# Check for Python
if ! command -v python3 &> /dev/null && ! command -v python &> /dev/null; then
    echo -e "${RED}Error: Python not found${NC}"
    echo "Please install Python 3.8 or later"
    exit 1
fi

PYTHON=$(command -v python3 || command -v python)

# Check for pymodbus
if ! $PYTHON -c "import pymodbus" 2>/dev/null; then
    echo -e "${YELLOW}Warning: pymodbus not found${NC}"
    echo "Installing pymodbus..."
    pip install pymodbus
fi

# Check for alr
if ! command -v alr &> /dev/null; then
    echo -e "${RED}Error: Alire (alr) not found${NC}"
    echo "Please install Alire from https://alire.ada.dev"
    exit 1
fi

# Cleanup function
cleanup() {
    echo ""
    echo "Stopping simulator..."
    if [ -n "$SIMULATOR_PID" ] && kill -0 "$SIMULATOR_PID" 2>/dev/null; then
        kill "$SIMULATOR_PID" 2>/dev/null || true
        wait "$SIMULATOR_PID" 2>/dev/null || true
    fi
}

trap cleanup EXIT

# Start simulator
echo "Starting Modbus simulator on port $PORT..."
cd "$SCRIPT_DIR"
$PYTHON modbus_simulator.py --port "$PORT" $VERBOSE &
SIMULATOR_PID=$!

# Wait for simulator to start
sleep 2

# Check if simulator is running
if ! kill -0 "$SIMULATOR_PID" 2>/dev/null; then
    echo -e "${RED}Error: Simulator failed to start${NC}"
    exit 1
fi

echo -e "${GREEN}Simulator started (PID: $SIMULATOR_PID)${NC}"
echo ""

# Build tests
echo "Building integration tests..."
cd "$PROJECT_ROOT"
alr build -- -P tests/integration/integration_tests.gpr

if [ $? -ne 0 ]; then
    echo -e "${RED}Build failed${NC}"
    exit 1
fi

echo -e "${GREEN}Build successful${NC}"
echo ""

# Run tests
echo "Running integration tests..."
echo "----------------------------------------"
"$PROJECT_ROOT/bin/integration_test_runner"
TEST_RESULT=$?
echo "----------------------------------------"

if [ $TEST_RESULT -eq 0 ]; then
    echo ""
    echo -e "${GREEN}All tests passed!${NC}"
else
    echo ""
    echo -e "${RED}Some tests failed${NC}"
fi

exit $TEST_RESULT

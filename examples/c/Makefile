# Makefile for C examples
# Builds C programs that link against the AdaModbus Ada library
#
# Usage:
#   make                  - Build all C examples
#   make modbus_client    - Build simple client example only
#   make clean            - Remove built files
#   make help             - Show available targets
#
# Note: Must be run from within Alire environment:
#   alr exec -- make
#
# On Windows (MINGW), ensure you have:
#   - GCC (MinGW-w64)
#   - Built Ada library: alr build
#   - GNAT adalib accessible

# Paths
PROJECT_ROOT := ../..
LIB_DIR := $(PROJECT_ROOT)/lib
OBJ_DIR := $(PROJECT_ROOT)/obj/c_examples
BIN_DIR := $(PROJECT_ROOT)/bin
C_API_DIR := $(PROJECT_ROOT)/src/c_api

# Compiler settings
CC := gcc
CFLAGS := -Wall -Wextra -std=c11 -g -O0
CPPFLAGS := -I$(C_API_DIR)

# Find GNAT adalib directory (contains libgnat.a)
# This assumes we're running within Alire environment
GNAT_PREFIX := $(shell dirname $(shell dirname $(shell which gnatmake 2>/dev/null)))
ADALIB_DIR := $(GNAT_PREFIX)/lib/gcc/x86_64-w64-mingw32/15.2.0/adalib

# Linker settings
# -ladamodbus: The Ada Modbus library
# -lgnat:      Ada runtime library
# -lws2_32:    Windows Sockets 2 library (for TCP)
LDFLAGS := -L$(LIB_DIR) -L$(ADALIB_DIR)
LDLIBS := -ladamodbus -lgnat -lws2_32

# All target executables
TARGETS := $(BIN_DIR)/c_tcp_master.exe \
           $(BIN_DIR)/c_tcp_slave.exe \
           $(BIN_DIR)/modbus_client.exe

.PHONY: all clean dirs help modbus_client

all: dirs $(TARGETS)
	@echo ""
	@echo "Build complete!"
	@echo "  $(BIN_DIR)/c_tcp_master.exe    - Interactive TCP master (client)"
	@echo "  $(BIN_DIR)/c_tcp_slave.exe     - TCP slave (server)"
	@echo "  $(BIN_DIR)/modbus_client.exe   - Simple read registers example"

# Convenience target for just the simple example
modbus_client: dirs $(BIN_DIR)/modbus_client.exe
	@echo ""
	@echo "Build complete: $(BIN_DIR)/modbus_client.exe"
	@echo ""
	@echo "Usage: modbus_client [host] [port] [slave_id] [start_address] [quantity]"
	@echo "Example: modbus_client 192.168.1.100 502 1 0 10"

dirs:
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(BIN_DIR)

# Pattern rule for compiling C files to object files
$(OBJ_DIR)/%.o: %.c $(C_API_DIR)/ada_modbus.h
	@echo "Compiling $<..."
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

# Linking rules for each executable
$(BIN_DIR)/c_tcp_master.exe: $(OBJ_DIR)/c_tcp_master.o
	@echo "Linking $@..."
	$(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

$(BIN_DIR)/c_tcp_slave.exe: $(OBJ_DIR)/c_tcp_slave.o
	@echo "Linking $@..."
	$(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

$(BIN_DIR)/modbus_client.exe: $(OBJ_DIR)/modbus_client.o
	@echo "Linking $@..."
	$(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

clean:
	rm -f $(OBJ_DIR)/*.o
	rm -f $(BIN_DIR)/c_tcp_master.exe
	rm -f $(BIN_DIR)/c_tcp_slave.exe
	rm -f $(BIN_DIR)/modbus_client.exe

help:
	@echo "AdaModbus C Examples - Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all            - Build all C examples (default)"
	@echo "  modbus_client  - Build simple client example only"
	@echo "  clean          - Remove built files"
	@echo "  help           - Show this help message"
	@echo ""
	@echo "Prerequisites:"
	@echo "  1. Build the Ada library first: alr build"
	@echo "  2. Run make within Alire environment: alr exec -- make"
	@echo ""
	@echo "Examples:"
	@echo "  alr exec -- make              # Build all examples"
	@echo "  alr exec -- make modbus_client  # Build simple example only"
	@echo ""
	@echo "Output files are placed in: $(BIN_DIR)/"

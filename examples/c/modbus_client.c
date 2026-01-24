/**
 * @file modbus_client.c
 * @brief Simple Modbus TCP Client Example for AdaModbus Library
 * @license MIT
 * Copyright (c) 2026 Florian Fischer
 *
 * This example demonstrates the basic usage of the AdaModbus C API:
 * - Creating a TCP connection
 * - Connecting to a Modbus TCP server
 * - Reading holding registers
 * - Proper error handling
 * - Cleanup
 *
 * Usage:
 *   modbus_client [host] [port] [slave_id] [start_address] [quantity]
 *
 * Defaults:
 *   host          = 127.0.0.1
 *   port          = 502 (standard Modbus TCP port)
 *   slave_id      = 1
 *   start_address = 0
 *   quantity      = 10
 *
 * Build:
 *   See Makefile in this directory
 *
 * Example:
 *   ./modbus_client 192.168.1.100 502 1 0 10
 *   (Reads 10 holding registers starting at address 0 from slave 1)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ada_modbus.h"

/*============================================================================
 * Configuration Defaults
 *============================================================================*/

#define DEFAULT_HOST          "127.0.0.1"
#define DEFAULT_PORT          502
#define DEFAULT_SLAVE_ID      1
#define DEFAULT_START_ADDRESS 0
#define DEFAULT_QUANTITY      10
#define CONNECTION_TIMEOUT_MS 5000
#define REQUEST_TIMEOUT_MS    3000

/*============================================================================
 * Helper Functions
 *============================================================================*/

/**
 * @brief Print usage information
 * @param prog_name Name of the program (argv[0])
 */
static void print_usage(const char* prog_name) {
    printf("Usage: %s [host] [port] [slave_id] [start_address] [quantity]\n\n", prog_name);
    printf("Arguments:\n");
    printf("  host          - Modbus server IP address (default: %s)\n", DEFAULT_HOST);
    printf("  port          - Modbus TCP port (default: %d)\n", DEFAULT_PORT);
    printf("  slave_id      - Target slave/unit ID, 1-247 (default: %d)\n", DEFAULT_SLAVE_ID);
    printf("  start_address - Starting register address (default: %d)\n", DEFAULT_START_ADDRESS);
    printf("  quantity      - Number of registers to read, 1-125 (default: %d)\n\n", DEFAULT_QUANTITY);
    printf("Example:\n");
    printf("  %s 192.168.1.100 502 1 0 10\n", prog_name);
    printf("  (Reads 10 holding registers starting at address 0 from slave 1)\n");
}

/**
 * @brief Convert modbus_status_t to a descriptive message and print it
 * @param status The status code to display
 * @param operation Description of the operation that produced this status
 * @return 0 if status is MODBUS_SUCCESS, -1 otherwise
 *
 * Note: modbus_status_string() returns a dynamically allocated string
 * that must be freed by the caller.
 */
static int check_status(modbus_status_t status, const char* operation) {
    const char* msg = modbus_status_string(status);

    if (status == MODBUS_SUCCESS) {
        printf("[OK] %s: %s\n", operation, msg);
        free((void*)msg);
        return 0;
    } else {
        fprintf(stderr, "[ERROR] %s: %s\n", operation, msg);
        free((void*)msg);
        return -1;
    }
}

/**
 * @brief Print register values in a formatted table
 * @param start_address First register address
 * @param values Array of register values
 * @param quantity Number of registers
 */
static void print_registers(uint16_t start_address, const uint16_t* values, uint16_t quantity) {
    printf("\n");
    printf("+---------+---------+--------+\n");
    printf("| Address |  Value  |  Hex   |\n");
    printf("+---------+---------+--------+\n");

    for (uint16_t i = 0; i < quantity; i++) {
        printf("| %7u | %7u | 0x%04X |\n",
               (unsigned)(start_address + i),
               (unsigned)values[i],
               (unsigned)values[i]);
    }

    printf("+---------+---------+--------+\n");
    printf("\n");
}

/*============================================================================
 * Main Program
 *============================================================================*/

int main(int argc, char* argv[]) {
    /* Configuration variables with defaults */
    const char* host      = DEFAULT_HOST;
    int         port      = DEFAULT_PORT;
    uint8_t     slave_id  = DEFAULT_SLAVE_ID;
    uint16_t    start_addr = DEFAULT_START_ADDRESS;
    uint16_t    quantity  = DEFAULT_QUANTITY;

    /* Handle type declarations */
    modbus_tcp_handle_t    tcp_handle    = NULL;
    modbus_master_handle_t master_handle = NULL;
    modbus_status_t        status;

    /* Return code (0 = success) */
    int result = 0;

    /* Buffer for register values */
    uint16_t values[125];  /* Max Modbus register read quantity is 125 */

    /*------------------------------------------------------------------------
     * Print header and library version
     *------------------------------------------------------------------------*/
    printf("=================================================\n");
    printf("  AdaModbus - Simple TCP Client Example\n");
    printf("  Library Version: %s\n", modbus_version());
    printf("=================================================\n\n");

    /*------------------------------------------------------------------------
     * Parse command line arguments
     *------------------------------------------------------------------------*/
    if (argc > 1) {
        /* Check for help flag */
        if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        }
        host = argv[1];
    }

    if (argc > 2) {
        port = atoi(argv[2]);
        if (port <= 0 || port > 65535) {
            fprintf(stderr, "Error: Invalid port number '%s'\n", argv[2]);
            return 1;
        }
    }

    if (argc > 3) {
        int sid = atoi(argv[3]);
        if (sid < 1 || sid > 247) {
            fprintf(stderr, "Error: Slave ID must be 1-247, got '%s'\n", argv[3]);
            return 1;
        }
        slave_id = (uint8_t)sid;
    }

    if (argc > 4) {
        int addr = atoi(argv[4]);
        if (addr < 0 || addr > 65535) {
            fprintf(stderr, "Error: Start address must be 0-65535, got '%s'\n", argv[4]);
            return 1;
        }
        start_addr = (uint16_t)addr;
    }

    if (argc > 5) {
        int qty = atoi(argv[5]);
        if (qty < 1 || qty > 125) {
            fprintf(stderr, "Error: Quantity must be 1-125, got '%s'\n", argv[5]);
            return 1;
        }
        quantity = (uint16_t)qty;
    }

    /* Print configuration */
    printf("Configuration:\n");
    printf("  Server:        %s:%d\n", host, port);
    printf("  Slave ID:      %u\n", (unsigned)slave_id);
    printf("  Start Address: %u\n", (unsigned)start_addr);
    printf("  Quantity:      %u registers\n\n", (unsigned)quantity);

    /*------------------------------------------------------------------------
     * Step 1: Create TCP connection handle
     *
     * The TCP handle manages the underlying socket connection.
     * It must be created before connecting.
     *------------------------------------------------------------------------*/
    printf("Step 1: Creating TCP connection handle...\n");
    tcp_handle = modbus_tcp_create();
    if (tcp_handle == NULL) {
        fprintf(stderr, "[ERROR] Failed to create TCP handle (out of memory?)\n");
        return 1;
    }
    printf("[OK] TCP handle created\n\n");

    /*------------------------------------------------------------------------
     * Step 2: Connect to Modbus TCP server
     *
     * This establishes the TCP socket connection to the server.
     * The connection timeout specifies how long to wait for the connection.
     *------------------------------------------------------------------------*/
    printf("Step 2: Connecting to %s:%d (timeout: %d ms)...\n",
           host, port, CONNECTION_TIMEOUT_MS);

    status = modbus_tcp_connect(tcp_handle, host, port, CONNECTION_TIMEOUT_MS);

    if (check_status(status, "TCP connect") != 0) {
        /* Print additional error details if available */
        const char* last_error = modbus_tcp_last_error(tcp_handle);
        if (last_error != NULL && strlen(last_error) > 0) {
            fprintf(stderr, "       Details: %s\n", last_error);
            free((void*)last_error);
        }
        result = 1;
        goto cleanup;
    }
    printf("\n");

    /*------------------------------------------------------------------------
     * Step 3: Create Modbus master (client) context
     *
     * The master handle manages the Modbus protocol layer on top of TCP.
     * Parameters:
     *   - tcp_handle: The TCP connection to use
     *   - MODBUS_MODE_TCP: Protocol mode (TCP/IP)
     *   - slave_id: Default slave/unit ID for requests
     *   - timeout: Default request timeout
     *------------------------------------------------------------------------*/
    printf("Step 3: Creating Modbus master context...\n");
    master_handle = modbus_master_create(tcp_handle, MODBUS_MODE_TCP,
                                         slave_id, REQUEST_TIMEOUT_MS);
    if (master_handle == NULL) {
        fprintf(stderr, "[ERROR] Failed to create master context\n");
        result = 1;
        goto cleanup;
    }
    printf("[OK] Master context created (default slave ID: %u)\n\n",
           (unsigned)slave_id);

    /*------------------------------------------------------------------------
     * Step 4: Read holding registers (Function Code 03)
     *
     * This sends a Modbus request to read holding registers and waits
     * for the response.
     *
     * Parameters:
     *   - master_handle: The master context
     *   - slave_id: Target slave/unit ID (1-247)
     *   - start_addr: Starting register address (0-based)
     *   - quantity: Number of registers to read (1-125)
     *   - values: Buffer to receive register values
     *   - timeout: Request timeout in milliseconds
     *
     * The function returns MODBUS_SUCCESS on success, or an error code.
     * If the slave returns a Modbus exception, the status will be one of
     * the MODBUS_EXCEPTION_* codes.
     *------------------------------------------------------------------------*/
    printf("Step 4: Reading %u holding registers from address %u...\n",
           (unsigned)quantity, (unsigned)start_addr);

    status = modbus_read_holding_registers(master_handle, slave_id,
                                           start_addr, quantity,
                                           values, REQUEST_TIMEOUT_MS);

    if (check_status(status, "Read holding registers") != 0) {
        result = 1;
        goto cleanup;
    }

    /*------------------------------------------------------------------------
     * Step 5: Display the results
     *------------------------------------------------------------------------*/
    printf("\nStep 5: Results:\n");
    print_registers(start_addr, values, quantity);

    /* Success! */
    printf("Operation completed successfully!\n");

/*------------------------------------------------------------------------
 * Cleanup: Always free resources in reverse order of creation
 *
 * Important: Even if an error occurs, we must clean up properly.
 * The goto cleanup pattern ensures resources are freed.
 *------------------------------------------------------------------------*/
cleanup:
    printf("\nCleaning up...\n");

    /* Destroy master context first (it uses the TCP handle) */
    if (master_handle != NULL) {
        modbus_master_destroy(master_handle);
        printf("  - Master context destroyed\n");
    }

    /* Disconnect TCP connection */
    if (tcp_handle != NULL) {
        modbus_tcp_disconnect(tcp_handle);
        printf("  - TCP disconnected\n");

        /* Destroy TCP handle last */
        modbus_tcp_destroy(tcp_handle);
        printf("  - TCP handle destroyed\n");
    }

    printf("Done.\n");
    return result;
}

# AdaModbus TLS for Embedded (mbedTLS)

TLS transport layer for embedded systems using mbedTLS. Designed for microcontrollers with constrained resources.

## Building

```bash
cd tls_mbed
alr pin adamodbus --use=..
alr build
```

## Features

- **Client and Server modes**: Connect to TLS servers or accept incoming connections
- **Certificate authentication**: X.509 certificates in DER format
- **PSK authentication**: Pre-shared keys for devices without certificate infrastructure
- **Hardware RNG integration**: Callback for true hardware entropy sources
- **Session resumption**: Cache TLS sessions for faster reconnects
- **Static memory allocation**: No malloc, compatible with ZFP/Light runtime

## Usage

### Client with Certificate Authentication

```ada
with Ada_Modbus.Transport.TLS_Mbed;

Config := (
   Mode            => Auth_Certificate,
   CA_Certificate  => CA_Cert'Address,
   CA_Cert_Len     => CA_Cert'Length,
   Certificate     => Client_Cert'Address,
   Certificate_Len => Client_Cert'Length,
   Private_Key     => Client_Key'Address,
   Private_Key_Len => Client_Key'Length,
   Verify_Peer     => True,
   others          => <>
);

Connect (Connection, "192.168.1.100", 802, Config, Result);
if Result = Success then
   Send_Frame (Connection, Request, Result);
   Receive_Frame (Connection, Response, Length, 5000, Result);
   Disconnect (Connection);
end if;
```

### Server with PSK Authentication

```ada
Config := (
   Mode         => Auth_PSK,
   PSK          => Shared_Key'Address,
   PSK_Len      => Shared_Key'Length,
   PSK_Identity => Identity'Address,
   PSK_Id_Len   => Identity'Length,
   others       => <>
);

Listen (Server, 802, Config, Result);
loop
   Accept_Connection (Server, Connection, 30000, Result);
   if Result = Success then
      --  Handle client requests...
      Disconnect (Connection);
   end if;
end loop;
```

## Memory Requirements

- Flash: 50-80 KB (depends on enabled features)
- RAM: 15-20 KB per TLS session

## Hardware RNG

For production use, provide a hardware entropy source:

```ada
Config.Entropy_Callback := My_Hardware_RNG'Access;
```

The callback should return true random bytes from a hardware RNG peripheral (e.g., STM32 RNG).

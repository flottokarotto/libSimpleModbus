# AdaModbus TLS for Desktop (AWS/OpenSSL)

TLS transport layer for desktop systems using AWS (Ada Web Server) with OpenSSL backend.

## Building

```bash
cd tls
alr pin adamodbus --use=..
alr build
```

## Requirements

- AWS (Ada Web Server) - pulled in automatically by Alire
- OpenSSL libraries installed on the system

## Usage

```ada
with Ada_Modbus.Transport.TLS;

--  Configure TLS with certificates
Config := (
   CA_File     => "certs/ca.crt",
   Cert_File   => "certs/client.crt",
   Key_File    => "certs/client.key",
   Verify_Peer => True
);

--  Connect to a Modbus/TCP Security server
Connect (Connection, "192.168.1.100", 802, Config, Result);
if Result = Success then
   --  Send Modbus request
   Send_Frame (Connection, Request, Result);

   --  Receive response
   Receive_Frame (Connection, Response, Length, 5000, Result);

   Disconnect (Connection);
end if;
```

## Certificate Files

The TLS transport expects PEM-formatted certificate files:

- `CA_File`: Certificate authority for server verification
- `Cert_File`: Client certificate (for mutual TLS)
- `Key_File`: Client private key

For testing, generate self-signed certificates:

```bash
# Generate CA
openssl req -x509 -newkey rsa:2048 -keyout ca_key.pem -out ca.crt -days 365 -nodes -subj "/CN=TestCA"

# Generate client certificate
openssl req -newkey rsa:2048 -keyout client.key -out client.csr -nodes -subj "/CN=ModbusClient"
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca_key.pem -CAcreateserial -out client.crt -days 365
```

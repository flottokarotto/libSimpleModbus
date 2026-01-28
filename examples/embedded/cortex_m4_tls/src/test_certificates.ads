--  Test Certificates for TLS Example
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  These are placeholder certificates for CI testing.
--  In production, replace with real certificates.
--
--  To generate test certificates:
--    # Generate CA
--    openssl req -x509 -newkey rsa:2048 -keyout ca_key.pem \
--      -out ca_cert.pem -days 365 -nodes -subj "/CN=TestCA"
--
--    # Convert to DER
--    openssl x509 -in ca_cert.pem -outform DER -out ca_cert.der
--
--    # Generate client certificate
--    openssl req -newkey rsa:2048 -keyout client_key.pem \
--      -out client_csr.pem -nodes -subj "/CN=ModbusClient"
--    openssl x509 -req -in client_csr.pem -CA ca_cert.pem -CAkey ca_key.pem \
--      -CAcreateserial -out client_cert.pem -days 365
--    openssl x509 -in client_cert.pem -outform DER -out client_cert.der
--    openssl rsa -in client_key.pem -outform DER -out client_key.der

with Ada_Modbus; use Ada_Modbus;

package Test_Certificates is

   pragma Preelaborate;

   --  Placeholder CA certificate (DER format)
   --  This is a minimal self-signed test certificate
   --  Replace with actual CA certificate for production
   CA_Cert : constant Byte_Array := (
      --  Placeholder: Minimal DER certificate structure
      --  In CI, this will be replaced by generated certificates
      16#30#, 16#82#, 16#01#, 16#00#,  --  SEQUENCE
      16#30#, 16#81#, 16#00#,          --  tbsCertificate (empty placeholder)
      16#30#, 16#00#,                   --  signatureAlgorithm (empty)
      16#03#, 16#00#                    --  signatureValue (empty)
   );

   --  Placeholder client certificate (DER format)
   Client_Cert : constant Byte_Array := (
      16#30#, 16#82#, 16#01#, 16#00#,
      16#30#, 16#81#, 16#00#,
      16#30#, 16#00#,
      16#03#, 16#00#
   );

   --  Placeholder client private key (DER format)
   Client_Key : constant Byte_Array := (
      16#30#, 16#82#, 16#01#, 16#00#,  --  SEQUENCE
      16#02#, 16#01#, 16#00#           --  version INTEGER 0
   );

end Test_Certificates;

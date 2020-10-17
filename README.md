# Agena

Agena is a minimal Gemini server written in Scheme, specifically
for the CHICKEN implementation.  It is based on Kooda's geminid[1],
but outsources TLS support to an external relay.  This allows the
server to be implemented as a simple gopher-like TCP server.

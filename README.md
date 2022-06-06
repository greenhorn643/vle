# vle
Variable length encoding of unsigned 64 bit integers

The purpose is to serialize small numbers compactly while still being able to serialize large values when needed.
This is achieved by packing 64 bit words into bytes where 7 bits represent data and the 8th bit is a flag indicating whether more bytes will follow.

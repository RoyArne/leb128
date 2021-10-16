# LEB128

Encode and decode integers in the LEB128 format.

# Overview

Provides functions to encode and decode integers to streams and arrays
using the signed and unsigned LEB128 format.

All of the functions expects to operate on integer values, streams and
vectors with element types `(unsigned-byte 8)`.

# Usage

**`(encode-signed integer &optional output start)`**

**`(encode-unsigned integer &optional output start)`**

Return two values, the encoded integer and the length of the encoding.

If `OUTPUT` is a `stream` then write the encoded `INTEGER` to it, preceding
the encoding with `START` number of zero bytes. Return `OUTPUT` and the
number of bytes written.

If `OUTPUT` is `NIL` then create a `(vector (unsigned-byte 8))` and encode
`INTEGER` into it. If `START` is given then precede the encoding with
`START` number of zero bytes. Return the vector, and its length.

If `OUTPUT` is a `(vector (unsigned-byte 8))` then encode `INTEGER` into it,
beginning at `START`. Return `OUTPUT` and the end index of the encoding.


**`(decode-signed input &optional start)`**

**`(decode-unsigned input &optional start)`**

Return two values, the decoded integer and the length of its encoding.

Return an integer decoded from `INPUT` beginning at `START`,
and the end index of the encoded value.

`INPUT` may be a `stream` or a `(vector (unsigned-byte 8))`.

If `INPUT` is a stream then `START` number of bytes is consumed and
discarded before the integer is decoded. The end index returned will
be the number of bytes consumed in total.


**`(signed-length integer)`**

**`(unsigned-length integer)`**

Return the number of bytes needed to encode `INTEGER`.

# License

Copyright (c) 2021 Roy Arne Gangstad.

Licensed under the  Apache License, Version 2.0.

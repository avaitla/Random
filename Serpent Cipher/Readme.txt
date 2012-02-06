Trying to Understand Serpent from http://www.cl.cam.ac.uk/~rja14/serpent.html. This is a smaller rewrite of the original version.


Serpent.c takes the S-BOXES directly from the standard implementation found with the specification, so that portion has the license included.

Also, encryption is slightly different than decryption for my implementation of serpent, since decryption produces a hex digest, and you need to give it that actual digest (so find the hex representation for the character), not the character representation alone. It is an easy fix, but I haven't implemented it since i was just gonna call it with only bytes from a different file. It passes the standard tests, which i used to verify the correctness.
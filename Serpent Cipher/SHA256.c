#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <inttypes.h>
#include <assert.h>

#define SHA256_BLOCK_LENGTH		64
#define SHA256_DIGEST_LENGTH		32

#if BYTE_ORDER == LITTLE_ENDIAN
#define REVERSE32(w,x)	{ \
	sha2_word32 tmp = (w); \
	tmp = (tmp >> 16) | (tmp << 16); \
	(x) = ((tmp & 0xff00ff00UL) >> 8) | ((tmp & 0x00ff00ffUL) << 8); \
}
#define REVERSE64(w,x)	{ \
	sha2_word64 tmp = (w); \
	tmp = (tmp >> 32) | (tmp << 32); \
	tmp = ((tmp & 0xff00ff00ff00ff00ULL) >> 8) | \
	      ((tmp & 0x00ff00ff00ff00ffULL) << 8); \
	(x) = ((tmp & 0xffff0000ffff0000ULL) >> 16) | \
	      ((tmp & 0x0000ffff0000ffffULL) << 16); \
}
#endif

#define R(b,x) 		((x) >> (b))
#define S32(b,x)	(((x) >> (b)) | ((x) << (32 - (b))))

// We use typedefs so that the machine figures
// out the proper types for us
typedef uint8_t  sha2_byte;	/* Exactly 1 byte */
typedef uint32_t sha2_word32;	/* Exactly 4 bytes */
typedef uint64_t sha2_word64;	/* Exactly 8 bytes */


typedef struct _SHA256_CTX {
	sha2_word32	state[8];
	sha2_byte	buffer[SHA256_BLOCK_LENGTH];
} SHA256_CTX;

// Initial Round Constants for SHA-256
const static sha2_word32 K256[64] = {
	0x428a2f98UL, 0x71374491UL, 0xb5c0fbcfUL, 0xe9b5dba5UL,
	0x3956c25bUL, 0x59f111f1UL, 0x923f82a4UL, 0xab1c5ed5UL,
	0xd807aa98UL, 0x12835b01UL, 0x243185beUL, 0x550c7dc3UL,
	0x72be5d74UL, 0x80deb1feUL, 0x9bdc06a7UL, 0xc19bf174UL,
	0xe49b69c1UL, 0xefbe4786UL, 0x0fc19dc6UL, 0x240ca1ccUL,
	0x2de92c6fUL, 0x4a7484aaUL, 0x5cb0a9dcUL, 0x76f988daUL,
	0x983e5152UL, 0xa831c66dUL, 0xb00327c8UL, 0xbf597fc7UL,
	0xc6e00bf3UL, 0xd5a79147UL, 0x06ca6351UL, 0x14292967UL,
	0x27b70a85UL, 0x2e1b2138UL, 0x4d2c6dfcUL, 0x53380d13UL,
	0x650a7354UL, 0x766a0abbUL, 0x81c2c92eUL, 0x92722c85UL,
	0xa2bfe8a1UL, 0xa81a664bUL, 0xc24b8b70UL, 0xc76c51a3UL,
	0xd192e819UL, 0xd6990624UL, 0xf40e3585UL, 0x106aa070UL,
	0x19a4c116UL, 0x1e376c08UL, 0x2748774cUL, 0x34b0bcb5UL,
	0x391c0cb3UL, 0x4ed8aa4aUL, 0x5b9cca4fUL, 0x682e6ff3UL,
	0x748f82eeUL, 0x78a5636fUL, 0x84c87814UL, 0x8cc70208UL,
	0x90befffaUL, 0xa4506cebUL, 0xbef9a3f7UL, 0xc67178f2UL
};

// Initial hash value H for SHA-256:
const static sha2_word32 sha256_initial_hash_value[8] = {
	0x6a09e667UL,
	0xbb67ae85UL,
	0x3c6ef372UL,
	0xa54ff53aUL,
	0x510e527fUL,
	0x9b05688cUL,
	0x1f83d9abUL,
	0x5be0cd19UL
};

void SHA256_INIT(SHA256_CTX* context)
{	
	// When you convert an integer value 0 to a pointer, it becomes a NULL pointer
	// The null pointer is guaranteed to compare unequal to a pointer to any object or function.
        // Conversion of a null pointer to another pointer type yields a null pointer of that type.
	// Any two null pointers shall compare equal.

	// Check Whether context is a Null Pointer	
	if (context == (SHA256_CTX*)0) { return; }

	// void * memcpy ( void * destination, const void * source, size_t num );
	// Copies the values of num bytes from the location pointed by source directly to the memory block pointed by destination.
	// The underlying type of the objects pointed by both 
	// the source and destination pointers are irrelevant for this function;
	// The result is a binary copy of the data.
	// The function does not check for any terminating null character in source - it always copies exactly num bytes.
	// To avoid overflows, the size of the arrays pointed by both the destination 
	// and source parameters, shall be at least num bytes, and should not overlap
	memcpy(context->state, sha256_initial_hash_value, SHA256_DIGEST_LENGTH);
	

	// void * memset ( void * ptr, int value, size_t num );
	// Sets the first num bytes of the block of memory pointed by ptr to the specified value (interpreted as an unsigned char).	
	memset(context->buffer, 0, SHA256_BLOCK_LENGTH);
}

void SHA256_Transform(SHA256_CTX* context, const sha2_word32* data)
{
	sha2_word32	a, b, c, d, e, f, g, h, s0, s1, maj, ch;
	sha2_word32	t1, t2, *W256;
	int		iterator;
	sha2_word32	Storage[SHA256_BLOCK_LENGTH];

	// Normall context->buffer is typed as an array of length SHA256_BLOCK_LENGTH (64) with 8 bit types (chars)
	// We are casting this into a pointer to 32 bit types, so there will be a total of 16 items pointable by W256
	// This essentiall splits the buffer into 16 32bit words 
	W256 = (sha2_word32*)(context->buffer);

	for(iterator = 0; iterator < 16; iterator++)
	{	// Endian Fixes, These methods convert Little Endian to Big Endian
		#if BYTE_ORDER == LITTLE_ENDIAN
			REVERSE32(*(data+iterator), W256[iterator]);
		#else
			W256[iterator] = *(data+iterator);	
		#endif
		Storage[iterator] = W256[iterator];
	}

	for(iterator = 16; iterator < 64; iterator++)
	{
		// s0 is a 32bit word
		s0 = S32(7,  Storage[iterator-15]) ^ S32(18, Storage[iterator-15]) ^ R(3, Storage[iterator-15]);
		s1 = S32(17, Storage[iterator-2]) ^ S32(19, Storage[iterator-2]) ^ R(10, Storage[iterator-2]);
		Storage[iterator] = Storage[iterator-16] + s0 + Storage[iterator-7] + s1;
	}

	/* Load Registers with initial 32 bit values */
	a = context->state[0];
	b = context->state[1];
	c = context->state[2];
	d = context->state[3];
	e = context->state[4];
	f = context->state[5];
	g = context->state[6];
	h = context->state[7];

	for(iterator = 0; iterator < 64; iterator++)
	{
		s0 = S32(2, a) ^ S32(13, a) ^ S32(22, a);
		maj = (a & b) ^ (a & c) ^ (b & c);
        	t2 = s0 + maj;
        	s1 = S32(6, e) ^ S32(11, e) ^ S32(25, e);
        	ch = (e & f) ^ ((~e) & g);
        	t1 = h + s1 + ch + K256[iterator] + Storage[iterator];
		h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
	}

	/* Compute the current intermediate hash value */
	context->state[0] += a;
	context->state[1] += b;
	context->state[2] += c;
	context->state[3] += d;
	context->state[4] += e;
	context->state[5] += f;
	context->state[6] += g;
	context->state[7] += h;

	/* Clean up */
	a = b = c = d = e = f = g = h = t1 = t2 = maj = ch = s0 = s1 = 0;
}

void SHA256_UPDATE(SHA256_CTX* context, const sha2_byte* data, size_t len)
{
	if(len == 0) { return; }
	if((context == (SHA256_CTX*)0) | (data == (sha2_byte*)0)) { return; }
	sha2_word64 totallength = len << 3;

	// Process as many complete blocks as we can
	while (len >= SHA256_BLOCK_LENGTH)
	{
		SHA256_Transform(context, (const sha2_word32*)data);
		len -= SHA256_BLOCK_LENGTH;
		data += SHA256_BLOCK_LENGTH;
	}

	// Now we need to pad the remainder. We must first pad it with 0x80.
	// If the new length is 56 bytes or less, then we just need
	// to allocate a temporary store of 8 bytes. Otherwise we need
	// 8 + 64 bytes. First we consider the case when the new length is
	// 56 bytes or less.
	int finalblocklength;
	if(len <= 55) { finalblocklength = 64; }
	else { finalblocklength = 128; }

	sha2_byte *finalblock = (sha2_byte *) malloc (finalblocklength);
	memset(finalblock, 0, finalblocklength);
	memcpy(finalblock, data, len);
	*(finalblock+len) = 0x80;


	// Now we need to proceed with preprocessing and padding
	#if BYTE_ORDER == LITTLE_ENDIAN
		REVERSE64(totallength, totallength);
	#endif
	*(sha2_word64 *)(finalblock + finalblocklength - 8) = totallength;

	// This Loop Will Run at Least Once and At Most Twice
	int iterator;
	for(iterator = 0; iterator < finalblocklength; iterator += SHA256_BLOCK_LENGTH)
		SHA256_Transform(context, (const sha2_word32*)(finalblock+iterator));
	free(finalblock);
}

char* SHA256_Data(const sha2_byte* data, size_t len, unsigned char buffer[SHA256_DIGEST_LENGTH])
{
	SHA256_CTX context;
	SHA256_INIT(&context);
	SHA256_UPDATE(&context, data, len);
	
	int i, i2;
	for(i=0; i!=8; i++)
		for(i2=0; i2!=4; i2++)
			buffer[4*i + i2]   = (unsigned char) (context.state[i] >> 24-8*i2);
	return buffer;
}

int main(int argc, char* argv[])
{
	if(argc!=2) return -1;
	unsigned char buffer[SHA256_DIGEST_LENGTH];
	SHA256_Data(argv[1], strlen(argv[1]), buffer);
	int i;
	for(i=0; i!=SHA256_DIGEST_LENGTH; i++)
		printf("%02x", buffer[i]);	
	return 0;
}

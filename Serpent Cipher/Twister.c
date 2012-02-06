#include <stdio.h>
#include <time.h>

unsigned int MT[624];

#define rotl32(a, n)	(((a)<<(n)) | ((a)>>(32-(n))))
#define rotr32(a, n)	(((a)>>(n)) | ((a)<<(32-(n))))
#define getbit(x, p) (((x) & ((unsigned int) 0x1 << (p))) >> (p))

void seed(unsigned int seed)	
{	MT[0] = seed;
	int i;
	for(i=1; i!=623; i++)
	MT[i] = (0x6c078965 * (MT[i-1] ^ rotr32(MT[i-1],30)) + i);	
}

void generateNumbers()
{
	int i;
	for(i = 0; i!= 624; i++)
	{
		unsigned int y = MT[i] & 0x80000000 + MT[(i+1)%624] & 0x7fffffff;
		MT[i] = MT[(i+397) % 624] ^ rotr32(y,1);
		if(y%2) { MT[i] ^= 0x9908b0df; }
	}
}

unsigned int random()
{
	static unsigned int pos = 0;
	if(pos == 0) { generateNumbers(); }
	unsigned int y = MT[pos];
	y ^= rotr32(y, 11);
	y ^= (rotl32(y, 7) & 0x9d2c5680);
	y ^= (rotl32(y, 15) & 0xefc60000);
	y ^= rotr32(y, 18);
	pos = (pos + 1) % 624;
	return y;
}

int main(int argc, char* argv[])
{	
	if(argc!=2) return -1;
	int i;
	seed((unsigned int)(time(NULL)));
	for(i=0; i<=atoi(argv[1])-1; i++) printf("%u, ", random());
	printf("%u", random());
}

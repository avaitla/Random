#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_DIGITS_PER_NODE 15

#define PLUS 1
#define MINUS -1

// The first node represents the least significant ints
// We connect nodes in the manner:
// NULL <- A_n <- ... <- A_2 <- A_1
// Note that Each A_i has MAX_DIGITS_PER_NODE
// So A_i.digits[MAX_DIGITS_PER_NODE] ++ ... ++ A_i.digits[1] ++ A_i.digits[0]

typedef struct _Number
{
	char digits[MAX_DIGITS_PER_NODE];
	struct _Number *next;
	struct _Number *previous;
	int blocknumber;
} Number;

typedef struct _SignedNumber
{
	Number* LeastSignificantBlock;
	Number* MostSignificantBlock;
	char signbit;			// 0 - Positive; 1 - Negative
	int HighestBit;
} SignedNumber;

void printSignedNumber(SignedNumber *num)
{
	if(num->signbit) printf("-");
	temp = num->MostSignificantBlock;
	while(temp->previous != NULL)
	{
		for(i = MAX_DIGITS_PER_NODE-1; i>=0; i--)
			printf("%d", temp->digits[i]);
		temp = temp->previous;
	}
	for(i = MAX_DIGITS_PER_NODE-1; i>=0; i--)
		printf("%d", temp->digits[i]);
}

void recursivePrint(Number *num)
{
	if(num->next != NULL)	recursivePrint(num->next);
	int i;
	for(i = MAX_DIGITS_PER_NODE-1; i>=0; i--)
		printf("%d", num->digits[i]);
}


char *getstring(char *buffer, int length)
{
	printf("Enter a Number: ");
	fgets(buffer, length, stdin);
}

Number *loadroutine(Number *num, const char *buffer)
{
	int j, i = 0;
	int offset;
	while(*(buffer+i) != '0')
	{	offset+=1;
		i+=1;
	}

	size_t length = strlen(buffer);
	char buffervalue;
	char *newstring;

	int bufferbits = 0;
	if((length-offset) % MAX_DIGITS_PER_NODE)
	{	
		//printf("Not Exact Fit Length (%d)\n", length);
		bufferbits = MAX_DIGITS_PER_NODE - length % MAX_DIGITS_PER_NODE;
		newstring = (char *) calloc(sizeof(char), length + 1 + bufferbits - offset);
		memset(newstring, 48, bufferbits);
		strcpy((newstring + bufferbits), buffer+offset);
	}

	else
	{	//printf("Exact Fit Length (%d)\n", length);
		newstring = (char *) calloc(sizeof(char), length + 1 - offset);
		strcpy(newstring, (buffer+offset));
	}

	length -= offset;
	
	while(buffervalue = *(newstring + i))
	{	//if(atoi(&buffervalue) > 9)
		//{	printf("Illegal Character (%c) at Position (%d).", *(buffer+i), i);			
		//	return;
		//}
		*(newstring + i) = (char) atoi(&buffervalue);
		//printf("%d", *(newstring+i));
		i+=1;
	}
	//printf("\n");

	int number_of_nodes = (length + bufferbits) / MAX_DIGITS_PER_NODE;
	Number *temporary, *head;
	head = NULL;

	int currentnodes = 1;
	Number *finalnode = num;	
	while(finalnode->next != NULL)
	{	
		finalnode = finalnode->next;
		currentnodes += 1;
	}	

	//if(currentnodes < number_of_nodes) { printf("Allocating %d New Nodes\n", number_of_nodes - currentnodes); }

	while(currentnodes < number_of_nodes)
	{
		temporary = (Number *)calloc(sizeof(Number), 1);
		temporary->next = NULL;
		temporary->previous = finalnode;
		temporary.blocknumber = finalnode.blocknumber + 1;		
		finalnode->next = temporary;
		finalnode = temporary;
		currentnodes += 1;
	}
	
	//for(i = 0; i != length+bufferbits; i++)
	//	printf("%d", *(newstring+i));
	
	Number *temp = num;
	for(i = 0; i != number_of_nodes; i++)
	{	for(j = 0; j != MAX_DIGITS_PER_NODE; j++)
			temp->digits[j] = newstring[(length+bufferbits - 1) - j - i*MAX_DIGITS_PER_NODE];
		temp = temp -> next;
	}
	free(newstring);
	return num;
}


SignedNumber *signed_loadroutine(SignedNumber *num, const char *buffer)
{
	const char *string = buffer;
	if(*buffer == '-')
	{	
		num->signbit = 1;
		string = buffer+1;
	}
	
	num->LeastSignificantBlock = loadroutine(num->LeastSignificantBlock, buffer);
	Number *temp = num->LeastSignificantBlock;
	while(temp->next != NULL) temp = temp->next;
	num->MostSignificantBlock = temp;

	int i = 0;	
	for(i = MAX_DIGITS_PER_NODE - 1; i>=0; i++)
		if(num->MostSignificantBit->digits[i])
		{
			num->HighestBit = 0;
			break;
		}
	return num;
}

void free_number(Number *num1)
{	
	if(num1->next == NULL) free(num1);
	else
	{
		Number *temp = num1->next;
		free(num1);
		free(temp);
	}
}

void free_signed_number(SignedNumber *num1)
{	free_number(num1->LeastSignificantBlock);	}

Number *allocate_number(Number *num1)
{
	num1 = (Number *)calloc(sizeof(Number), 1);
	num1->next = NULL;
	num1->previous = NULL;
	num1->blocknumber = 0;
	return num1;
}

SignedNumber *allocate_signed_number(SignedNumber *num1)
{
	num1 = (SignedNumber *)calloc(sizeof(Number), 1);
	Number *temp1;
	temp1 = allocate_number(temp1);
	num1->LeastSignificantBlock = temp1;
	num1->MostSignificantBlock = temp1;
	return num1;
}

void testing()
{
	char buffer1[1024];
	char buffer2[1024];
	memset(buffer1, 0, 1000);
	memset(buffer2, 0, 1000);
	getstring(buffer1, 1000);
	getstring(buffer2, 1000);
	buffer1[strlen(buffer1) - 1] = '\0';
	buffer2[strlen(buffer2) - 1] = '\0';

	// Now we Load these into our Structures;
	Number *Number1, *Number2;
	Number1 = allocate_number(Number1);
	Number2 = allocate_number(Number2);


	Number1 = loadroutine(Number1, buffer1);
	Number2 = loadroutine(Number2, buffer2);

	recursivePrint(Number1);
	printf("\n");
	recursivePrint(Number2);
	printf("\n");
}

void add_number(Number *num1, Number *num2, Number *num3)
{
	int carry = 0;
	
}

void subtract_number(Number *num1, Number *num2, Number *num3)
{

}

int compare_number(Number *num1, Number *num2)
{

	return 0;
}

void multiply_number(Number *num1, Number *num2, Number *num3)
{
	

}

void digit_shift(Number *num1, Number *num2, int d)
{

}

void divide_number(Number *num1, Number *num2, Number *num3)
{



}

void exponentiate_number(Number *num1, Number *num2, int d)
{

}

Number *int_to_number(int num1, Number *num2)
{
	unsigned int value = abs(num1);
	int digit_counter = 0;
	while(value){ digit_counter+=1; value /= 10; }
		
	char *string = (char *)calloc(sizeof(char), digit_counter+1);
	sprintf(string, "%d", abs(num1));	
	num2 = loadroutine(num2, string);
	return num2;
}

SignedNumber *int_to_signednumber(int num1, SignedNumber *num2)
{
	if(num1 < 0) num2->signbit = 1;	
	num2->LeastSignificantBit = int_to_number(num1, num2->LeastSignificantBit);
	
}

int main()
{
	Number *num;
	num = allocate_number(num);	
	num = int_to_number(123344567, num);
	recursivePrint(num);

//	testing();
}

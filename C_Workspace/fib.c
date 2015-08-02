#include <stdio.h>

/** \breif computes nth number in the Fibonacci sequence.
*   \param n Index of sequence.
*   \return nth number in sequence.
*   \note This function uses inline x86 assembly.
*/
int fib(int n)
{
	int fib = 0;

	_asm
	{
		mov ecx, n

		mov eax, 0		;a=0
		cmp ecx, 0
		je finish		;If n=0, ret 0
		mov ebx, 1		;b=1

	fibloop:
		mov edx, eax
		add edx, ebx		;EDX=a+b
		mov eax, ebx		;a=b
		mov ebx, edx		;b=a+b
		loop fibloop		;ret a when n=0

	finish :
		mov fib, eax
	}

	return fib;
}

int main()
{
	int i = 0;
	for (i; i < 15; ++i)
		printf("fib(%d)= %d\n", i, fib(i));
	/* fib(0)=0, fib(1)=1, fib(2)=1...	*/

	getchar(); //Used to halt execution.
}

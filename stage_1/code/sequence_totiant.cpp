#include <stdio.h>
#include <time.h>
// hcf x 0 = x
// hcf x y = hcf y (rem x y) 计算x,y的最大公约数
long hcf(long x, long y)
{
    long t;
    while (y != 0)
    {
        t = x % y;
        x = y;
        y = t;
    }
    return x;
}

// relprime x y = hcf x y == 1
//判断x,y两个数的最大公约数是否为1
int relprime(long x, long y)
{
    return hcf(x, y) == 1;
}
// euler n = length (filter (relprime n) [1 .. n-1])
//计算某个数的欧拉数
long euler(long n)
{
    long length, i;
    length = 0;
    for (i = 1; i < n; i++)
        if (relprime(n, i))
            length++;
    return length;
}
// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])
//计算从x到y之间所有数的欧拉函数的和
long sumTotient(long lower, long upper)
{
    long sum, i;
    sum = 0;
    //每个数都求欧拉数，然后累加
    for (i = lower; i <= upper; i++)
        sum = sum + euler(i);
    return sum;
}
int main()
{
    long lower = 1;
    long upper = 2500;

    clock_t start, finish;
    double duration = 0;
    start = clock();

    printf("C: Sum of Totients  between [%ld..%ld] is %ld\n",
           lower, upper, sumTotient(lower, upper));
    finish = clock();
    duration = ((double)finish - start) / CLOCKS_PER_SEC;
    printf("time----- %f s", duration);
    return 0;
}

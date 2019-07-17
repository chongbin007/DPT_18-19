package main

import (
	"fmt"
	"time"
)

// Compute the Highest Common Factor, hcf of two numbers x and y
//
// hcf x 0 = x
// hcf x y = hcf y (rem x y)

func hcf2(x, y int64) int64 {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	return x
}

// relprime determines whether two numbers x and y are relatively prime
//
// relprime x y = hcf x y == 1

func relprime2(x, y int64) bool {
	return hcf2(x, y) == 1
}

// euler(n) computes the Euler totient function, i.e. counts the number of
// positive integers up to n that are relatively prime to n
//
// euler n = length (filter (relprime n) [1 .. n-1])

func euler2(n int64) int64 {
	var length, i int64

	length = 0
	for i = 1; i < n; i++ {
		if relprime2(n, i) {
			length++
		}
	}
	return length
}

// sumTotient lower upper sums the Euler totient values for all numbers
// between "lower" and "upper".
//
// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

func sumTotient2(lower, upper int64, c chan int64) {
	var sum, i int64

	sum = 0
	for i = lower; i <= upper; i++ {
		sum = sum + euler2(i)

	}
	c <- sum //计算出sum把sum传给channel
	fmt.Println("Sub threads : sum of", lower, "and", upper, "is: ", sum)

}

func main() {
	c := make(chan int64) //创建channel，用来作为通信通道

	var numberOfThread int64 = 8 //创建线程数
	var upper int64 = 15000
	var lower int64 = 1

	intervalNum := upper / numberOfThread //把总和分成线程数个段来执行

	// Record start time
	start := time.Now()
	//循环分配线程，每个线程执行一段计算
	//比如 8000分四段 ：1-2000， 2001-4000，4001-6000，6001-8000
	var i int64
	for i = 1; i <= numberOfThread; i++ {
		//开启go线程并把指定c通道作为main和go线程通道
		go sumTotient2(1+intervalNum*(i-1), intervalNum*i, c)
	}

	//取得结果：通过c通道来获得每个个线程的结果值
	var j int64
	var totalNum int64 = 0 // 计算结果总和
	var subTotal int64 = 0 //每个线程的计算结果
	for j = 1; j <= numberOfThread; j++ {
		subTotal = <-c //通过c取得每个线程的计算结果
		//fmt.Println(subTotal)
		totalNum = subTotal + totalNum //把计算结果相加得到最好结果
	}
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", totalNum)

	// Record the elapsed time
	t := time.Now() //挺会计时
	elapsed := t.Sub(start)
	fmt.Println("Number of go routines is ", numberOfThread, "----Elapsed time is ", elapsed)
}

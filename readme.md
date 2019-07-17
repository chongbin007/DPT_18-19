## 说明
该项目为**Distribute and parallel programming**课程作业，使用不同三种编程语言来开启多线程实现欧拉数求和的功能。开启多线程来运行程序可以让求和速度更快，达到更高效的目的。具体项目任务详见内部任务文件。
## 进入gpgnode-01方法
首先远程登录自己的账户
然后ssh gpgnode-01   输入密码
查看核心数（8核）
`cat /proc/cpuinfo`


## 运行C程序
`gcc -Wall -o hello hello.c `  //hello是生成的编译文件名称，可以修改
`./hello   `            //运行C文件

运行DS1然后等待运行时间

### 运行 OPENMP  
必须用打开OPENMP的命令
`gcc -fopenmp -o hello -O2 omp_hello.c`	
`./hello`

## 运行Go程序
先导入环境变量，因为go 已经安装
`% export PATH=$PATH:/usr/local/go/bin`
然后运行文件
`go run seq.go`

## 运行erlang 
需先进入gpgnode，已经安装好erl
当你创建一个tut.erl文件时候，每个文件夹对应创建一个相同名称的文件，然后通过terminal进入到这个文件夹下再直接运行  进入erlang shell
erl

编译
`> c(seq).`	
	
输出ok
然后输入module名称和调用方法名称

` > seq:sumTotient(1,5000).`








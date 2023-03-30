# erlang-project

## Overview

Create a set of distributed communicating processes that communicate using
the RIP protocol and compute primes.
• Each object will have a unique human readable identifier called the
nickname. Each nickname will map to an Erlang Process ID (PID);
• Each process will have between one and three direct connections to
other processes. These are known as immediate neighbors. Communication between immediate neighbors uses the standard Erlang
PIDs;
• Each process will only store/know the PIDs of its immediate neighbors. All other communication will use only the nickname;
• If a process needs to send a message to another process that is not an
immediate neighbors then it must use the basic Routing Information
Protocol (RIP)1
to do so;
• Each process can respond to two messages:

1. computeNthPrime;
2. receiveAnswer

## Building

• Ensure Erlang is installed on your local Device
• Ensure you are in the "src" Directory

```
erl -make
```

## Testing

First enter the Erlang Command prompt with the following command:

```
erl
```

Then Run the test

```
test:run().
```

## Directory Structure

my_project/
├── src/
│ ├── main.erl
│ ├── node.erl
│ ├── routing.erl
│ └── utils.erl
├── include/
│ └── header.hrl
├── tests/
│ └── test.erl
└── README.md

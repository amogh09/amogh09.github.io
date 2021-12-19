---
layout: post
title: TCP Gender Changer with Socat
date: 2021-12-16
tags: tcp socat tcp-gender-changer
---
In today's post we will be talking about Gender Changing. Huh? Gender Changing? Is this still a programming blog? Yes, it is. We'll be looking at TCP Gender Changer today along with a simple Socat based implementation of it.

# Problem
You have a server running inside a zone protected by a firewall that needs to be accessed by clients from outside the firewall. The server must run inside the protected zone and it cannot directly accept inbound connections from outside. This is a common situation for non-public facing sensitive systems such as those used by Finance companies for accepting connections from other companies.

![Scenario](/assets/tcp-gender-changer/tcp-gender-changer.001.jpeg)

# Solution - TCP Gender Changer
One of the solutions to this problem is a mechanism known as TCP Gender Changer. You can read about it in more detail on [Wikipedia](https://en.wikipedia.org/wiki/TCP_Gender_Changer) but the basic idea is to have a process inside the Firewall initiating connections to another process outside the Firewall and also to the Server. The process outside the Firewall is responsible for accepting inbound client connections and then relaying them to outbound connections it received from the other process inside the Firewall. I know it's a bit confusing so let's break down the mechanism into steps.

# Step-by-step mechanism 
1. A process is started on a host outside the Firewall. This process is called a Listen-Listen or LL node.
1. A process is started on a host inside the Firewall. This process is called a Connect-Connect or CC node. 
1. The LL node listens for connections from external clients as well as for connections from CC node, that's why it's called Listen-Listen.
1. The CC node establishes a connection to the LL node and waits for a signal from it about a new client connection. 
1. An external client connects to the LL node. LL node sends a signal to the CC node on its pre-established connection.
1. The CC node receives the signal about new client connection and initiates a new connection to the server.
1. Server accepts the connection from CC node.
1. LL node and CC node together form a relay between the Client and the Server. Data transfer can now occur freely between the Client and the Server.

![Sequence Diagram](/assets/tcp-gender-changer/seq.png)

# Implementation with Socat
We can implement a simple TCP Gender Changer using the [Socat](https://linux.die.net/man/1/socat) utility for Unix based systems. Quoting from the Socat manual, Socat is a command line based utility that establishes two bidirectional byte streams and transfers data between them. 

#### LL Node
To setup the LL Node, we use the following command to start a Socat process.

```bash
$ socat -d -d -d tcp-l:8000,reuseaddr,fork tcp-l:8001,reuseaddr
```

The command above tells Socat to first listen for client connections on port 8000 and then on port 8001. Port 8000 is for external clients whereas port 8001 is for the CC Node. The `fork` option when listening on port `8000` will make Socat spawn a new child process for listening on port `8001` each time it accepts a connection from external client on port `8000`. This way we will be able to handle an arbitrary number of clients.

#### CC Node
We start a CC Node using the following Socat command. Here we assume that the server is listening for connections on port 7200.

```bash
$ socat -d -d -d tcp:<LL-node-address>:8001,forever,interval=2,fork tcp:<server-address>:7200
```

This command tells socat to first establish a connection to the LL Node on port 8001 and then establish a connection to the server on port 7200. 

Since, the LL Node is configured to accept a client connection before listening for a connection from the CC Node, connection attempts from CC Node to LL Node will fail until the latter receives a client connection. For this reason we tell the CC Node Socat to retry connection attempts to LL Node forever with a sensible interval between attempts (2 seconds in the example above). Only when a connection attempt to LL Node succeeds will the CC Node connect to the Server.

#### In action 
Let's test if the above setup works as expected. For simplicity of our test we wil run both LL and CC nodes as well as the server locally. 

**Server**

For the server I am going to use the default Python HTTP Server from `http.server` module. This server serves static files in the current directory.

```bash 
$ python3 -m http.server 7200
```

**LL Node**
```bash
$ socat -d -d -d tcp-l:8000,reuseaddr,fork tcp-l:8001,reuseaddr
```

**CC Node** 
```bash
$ socat -d -d -d tcp:127.0.0.1:8001,forever,interval=2,fork tcp:127.0.0.1:7200
```

Now, if you open `localhost:8000` in your web browser you will see the HTTP Server return a result containing all the files in its current directory.

![Server result](/assets/tcp-gender-changer/server-result.png)
What's difficult about this project?
====================================

Every time I try to explain what's so hard about writing this network application, I don't really remember the difficulties with enough detail to explain them.




Debugging is awful
------------------

Due to the nature of the network, a single client is mostly meaningless. This makes debugging hard: things cannot very easily be observed in isolation.

1. I had a case where I wanted to reduce communication between nodes to a minimum to filter out a bad signal (well-typed, but issued at the wrong time to an unsuspecting neighbour). The problem with disabling all other messages meant that nodes don't send keep-alive messages anymore, making downstream neighbours unable to detect whether their upstream partners were dead; the upstream contingent of nodes kept filling, until no new connections could be accepted. At this point the network structure breaks down, and the behaviour of the entire network is undefined.

2. A memory+socket+thread leak that took me months to fix seemed to form only after speeding up the bootstrap server by a factor of ten, and then letting it run for over ten minutes. Simple changes, along with running the program for ten minutes again to see their effects, often improved - but never fixed - the issue. There were many cases in which the development cycle was just excruciatingly long.

3. It is often not sufficient to identify one bug (by accident while reading the code again to my cat, for example) to fix the program: when multiple bugs come together, "fixing" one of them may not be enough to observably improve anything. After correcting something, this left me in odd situations:

    - Did I really fix the bug, or just change some code, potentially introducing new bugs that will bite me later?

    - The program didn't change; did I simply waste my time?

    - The code seems conceptually right to me now after the change; am I thinking about the whole thing the wrong way, now that it didn't help?

    - Should I roll back my change and try fixing something else instead, hoping to modify only small code pieces so I can pin down the error location? Yes? Work lost, potentially good fix lost. No? See previous points.

4. Getting good logs is next to impossible. Reading a single node's log is easy and clear, but does not show what other nodes thought when inducing this behaviour. Reading all logs at the same time produces a large amount of output in which it is very hard to tell cause, effect and correlation and random chance apart.





Emergent properties
-------------------

Although the individual rules of communication are simple, multiple nodes together develop what is known as emergent behaviour: complex patterns that are not obvious by merely looking at the rules the individual follows. An example of this are timing problems: when nodes don't time out fast enough, new connections may not be able to be formed in due time, leaving nodes orphaned sometimes. An orphan does not know of any neighbours, so it is lost; the only solution in this case is a full restart of the node (equivalent to killing and replacing it).





Bootstrapping
-------------

To create a network nodes have to connect to it in order to create a network nodes have to connect to it in order to create ...

You get the gist. The current solution is to have a bootstrapping server with special "gullible" nodes that act like normal nodes, but take instructions from their parent without questioning. When starting the bootstrap server, this so-called node pool will be very interconnected, because there is no network other than itself yet. Since the network structure is static, this interconnectedness stays the same even after many independent nodes are connected; the node pool fills up and many requests never bounce outside of the bootstrap server's sub-network. This is of course undesirable.

In order to avoid this problem, the bootstrapping server restarts single nodes in its pool periodically, so that they can form fresh connections to nodes outside of the pool; the goal is having the node pool be connected to very distant parts of the network to avoid clustering. But when should this restart happen?

- Periodically: When a lot of nodes connect within one period, the pool's sub-network fills up all the way, making it difficult to form new connections to the network.

- Every `n` new nodes: Suppose the pool has `k` nodes; when `k*n` new clients connect simultaneously, the server restarts its entire pool at once and loses its connection to the network.

The best solution is probably a hybrid: restart a node on every `n` new clients, require a period of at least `t` to pass since the last event.
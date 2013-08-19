Amoeba
======

Amoeba is a program for setting up a decentralized network. The name comes from the hope that eventually, the network will be so robust that you can poke any of its parts without endangering its overall integrity.

**Amoeba is currently in the phase that comes before Alpha (coding, compiling, but not running it because it's full of placeholders).**



Planned features
----------------

- Every node only knows about its immediate neighbours. Unless explicitly added, the origin of a signal sent over the network is untraceable.

- Highly concurrent nodes (one thread per connection) using message passing to communicate.

- Robustness: Network integrity should be maintained even in case of:

    - Bad/too long/too many requests

    - Failure of a comparatively small amount of nodes

    - Malicious minorities



Research goals
--------------

- How many neighbours does each node need so the network isn't clustered/destroyed even when large numbers of nodes go offline?

- How large of a network can a single RasPi handle? :-)

- How long does a new message need to reach the entire network?

- What does the network look like after some time?

- How can certain attacks on the network be prevented?



Network description
-------------------

An outline of how the network looks like:

- All nodes run identical programs.

- Each node has a number of upstream and downstream neighbours, which are allowed to send data to the current node, or accept data sent by it, respectively. It has no knowledge about the network other than its neighbours.

- Nodes have a minimum and maximum number of neighbours for both upstream and downstream connections (independent of each other). If there is a deficit of connections, nodes will request new neighbours from the network; if there is a surplus, no new connections will be accepted; if there is neither, no requests will be sent, but incoming requests will be processed.

- If a node has a deficit in connections, it will randomly tell one of its neighbours of it. This is called an EdgeRequest, and contains its own address, and parameters determining how far the request should travel through the network. The EdgeRequest is relayed by receiving nodes a number of times, passing it on to one of their own downstream neighbours, until eventually one of them accepts the request, and establishes the desired connection with the initially issuing node.

- Initial connection is made using a specialized bootstrap server, which has a known address. This server is not part of the network, it only serves to point new clients to existing nodes.

- To look at the large scale structure of the network, a specialized request can be made by a specialized graph plot server. This request makes every client send a list of all its neighbours to the plot server. (This is strictly a debugging tool, since it opens the door for a truckload of attacks.)


Known vulnerabilities and immunities
------------------------------------

This is a list of known and feasible attacks on the current design:

- Malicious single nodes

  - DDoS: Spamming the network with loads of trashy messages. Especially flooding signals (such as text messages) have a very large impact compared to the single signal they are issued with. Furthermore, messages are anonymous, so if you have N neighbours, you can at least send N messages per malicious node without any possible spam protection being able to jump in. (This attack does of course scale with the number of spammers.)

  - SlowLoris

  - Node crawling: Although nodes only retain the addresses of downstream neighbours (remember the upstream connection is one-way, clients will not send or handle signals issued the wrong way), EdgeRequest signals carry valid server addresses and traverse a large part of the network before they are accepted. Specialized nodes could simply store all valid addresses they encounter. This is completely undetectable by other nodes and, while not dangerous on its own, can lead to a large knowledge about the network. The node database could be shared with malicious nodes that could harness that information for attacks on the network.

- Malicious sub-networks

  - Nodes accepting all EdgeRequest signals they encounter can build up a connection to a large portion of the network

    - Filtering: Certain signals could be thrown away, for example EdgeRequest signals from a certain IP range so that no new nodes can connect to the network

    - Altering signals

    - No network dynamics: The network right now is static, once a connection is established it will only be destroyed due to technical failure or planned termination. A test where two neighbours compare some of their neighbours and drop common ones could battle the possibility of nodes gathering up too many connections illegally.

  - *Immune:* Bootstrap takeover: The bootstrap server remembers all nodes it has helped spawn. Should the malicious network be able to bully out all legally known nodes, the bootstrap server would have to send the next request through the malicious network first. However, the newly spawned node now exists in the bootstrap server's database, providing a way through the blockade.


Terminology
-----------

- _foo: Accessor functions that don't do any computation otherwise
- DSN:  Downstream node, i.e. a neighbouring node the current sends commands do.
- ST1C: Server to one (unspecified/arbitrary) client channel
- STC:  Server to client channel
- STSC: Server to specific client channel
- USN:  Upstream node, i.e. a neighbouring node the current gets commands sent by.
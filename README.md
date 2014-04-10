Amœba
======

Amœba is a program for setting up a decentralized network. The name comes from
the hope that eventually, the network will be so robust that you can poke any of
its parts without endangering its overall integrity.

This is how a small network looks like. It consists of 359 nodes, each trying to
maintain 5-10 incoming and outgoing connections. Initially, three independent
clusters were created; the last 30 nodes knew of these clusters and joined them
together over the course of a minute. Darker colours indicate a more central
role of a node in the network ([betweenness centrality][wiki-betweenness]).
Click for higher resolution.

[![(Picture missing, uh oh)](doc/network_example.png)][network-hires]

[network-hires]: doc/network_example_hires_amoeba_n359_5-10-3cluster.png



The current development stage is Alpha.

Branch  | Status
:-----: | :-----:
master  | [![(Travis image broken)](https://travis-ci.org/quchen/amoeba.png?branch=master)][travis]
develop | [![(Travis image broken)](https://travis-ci.org/quchen/amoeba.png?branch=develop)][travis]



[travis]: https://travis-ci.org/quchen/amoeba
[wiki-betweenness]: http://en.wikipedia.org/wiki/Betweenness_centrality





Explanation in simple terms
---------------------------

(This section is for you if you're wondering what the fuzz about the above crumbled up piece of strings and dots is.)

I was always superficially fascinated by complex systems and networks, most notably by what is called *emergence*: the appearance of complex behaviour in systems made up from simple rules. A single ant does not do anything complex, and neither do ten of them. Put a thousand together though and you will discover that, although each is still individually doing the same things as before, it will amount to something much bigger than what you would have expected from the individual: complex structures of air tunnels or [fungus farms][leafcutter]. Another example is any living organism: even if you understood how every cell worked exactly, you still would have no idea about whether (or why) putting them together in some way can make up the organism that I am right now, typing this paragraph.

Networks also exhibit a lot of emergent properties, and contrary to living organisms they are much more suitable to being simulated and applied by computers. A network in this sense is simply a number of constituents with connections to other constituents. These networks can consist of people (where conncetions can be "who likes who" or "have met each other at some point"), computers ("connected over the internet", "contains parts made by the same manufacturer"), languages ("what words can be used after others") and many other things.

Amœba is a program that creates a computer network. I came up with the idea around the time of the first Bitcoin boom in 2013; the Torrent network did also seem somewhat interesting to me. So I thought "why not implement a basic version of something like that yourself?" - generously estimating 500 lines of code to get the core done. Months and thousands of lines of code added/removed/edited later, a satisfying first version is still just barely on the horizon, but it's finished enough to be able to play around with it. The "crumbled up piece of strings and dots" above is a snapshot of an Amœba network, a few seconds before I terminated half of it to see whether it would survive that without clustering into many disconnected components. Research has begun! :-)

[leafcutter]: https://en.wikipedia.org/wiki/Leaf_cutter_ant







Planned features
----------------

- Anonymity: every node only knows about its immediate neighbours. Unless
  explicitly added, the origin of a signal sent over the network is untraceable.

- The network should be as robust as possible against malicious participants.

- Even large network outages should not lead to disconnected components.

- Neat live graph drawing to have a global view of the entire network (or at
  least the nodes choosing to be published).

Also see the [issues list][issues] on GitHub.

[issues]: https://github.com/quchen/amoeba/issues





Research goals
--------------

- Structure

  - What node degrees are necessary for a robust network?

  - How long, if possible, does it take an almost dead node to heal again?

  - How does the network diameter scale with number of participants? This is
    important to ensure the shortest paths between arbitrary nodes stay short,
    ensuring fast message delivery.

  - What are the timescales for bootstrapping the network, adding or removing
    one node or many nodes?

  - How do two disconnected network components meld together when a connection
    is introduced?

- Integrity

  - What attacks can be prevented by design?

  - Worst case scenarios: what are malicious participants allowed to do?


These goals are subject to certain constraints:

- A node only knows its immediate neighbours. (No global knowledge hacks.)

- Network dynamics are static: a link is only broken by technical failure, and
  not as part of the network dynamics.





Network description
-------------------


![(Picture missing, uh oh)](doc/network_schema.png
                            "Network structure of a small system")

The picture shows the network structure of a small Amœba network. Blue arrows
are ordinary connections, while red ones stand for local direct connections,
used by special network services.



### Normal nodes

- All nodes run identical programs.

- Each node has a number of upstream and downstream neighbours, which are
  allowed to send data to the current node, or accept data sent by it,
  respectively. It has no knowledge about the network other than its neighbours.

- Nodes have a minimum and maximum number of neighbours for both upstream and
  downstream connections (independent of each other). If there is a deficit of
  connections, nodes will request new neighbours from the network; if there is a
  surplus, no new connections will be accepted; if there is neither, no requests
  will be sent, but incoming requests will be processed.

- If a node has a deficit in connections, it will randomly tell one of its
  neighbours of it. This is called an *edge request*, and contains its own
  address, and parameters determining how far the request should travel through
  the network. The edge request is relayed by receiving nodes a number of times,
  passing it on to one of their own downstream neighbours, until eventually one
  of them accepts the request, and establishes the desired connection with the
  initially issuing node.

- Nodes will attempt to minimize the number of connections above the minimum by
  *pruning*. They will do so by telling downstream neighbours of their wish to
  drop the connection, which will be accepted if this can be done without making
  the partner go below the minimum limit.

- Initial connection is made using a special bootstrap service, see the section
  below.

- To look at the large scale structure of the network, a special request can be
  made by a special graph plot server. This request makes every client send a
  list of all its neighbours to the plot server. (This is strictly a debugging
  tool, since it opens the door for a truckload of attacks.)



### Special services

A central point in node design is that they reject signals from unregistered
origins, so that spamming a single node from outside does not affect the network
at all.

However, this is sometimes too restrictive: for some services, it makes sense to
be able to issue signals, despite them not being part of the network. To solve
this problem, nodes can be spawned with a special direct communication channel
that can be used to send messages to it directly.



#### Bootstrap server

A bootstrap server is the first contact a node makes after startup, and issues
edge requests on behalf of its client.



#### Drawing server

The drawing server's purpose is creating a map of the network to study its
structure. Issues a signal that makes every (willing) node of the network send
it a list of their downstream neighbours.





Known vulnerabilities and immunities
------------------------------------

This is a list of known and feasible attacks on the current design:

- Malicious single nodes

  - DDoS: Spamming the network with loads of trashy messages. Especially
    flooding signals (such as text messages) have a very large impact compared
    to the single signal they are issued with. Furthermore messages are
    anonymous, so if you have N neighbours, you can at least send N messages per
    malicious node without any possible spam protection being able to jump in.

  - Spamming the bootstrap server with requests yields an arbitrary amount of
    new edge requests, allowing a node to quickly connect to a large part of the
    network.

  - The drawing server capabilities are currently available to everyone, and not
    just legitimate drawing servers. In other words, getting a list of all nodes
    is trivial.

  - *Resistent*: SlowLoris attacks. Since signal sizes are relatively small,
    connection timeouts can be short.

  - Node crawling: Although nodes only retain the addresses of downstream
    neighbours (remember the upstream connection is one-way, clients will not
    send or handle signals issued the wrong way), edge requests carry valid
    server addresses and traverse part of the network before they are accepted.
    Specialized nodes could simply store all valid addresses they encounter.
    This is completely undetectable by other nodes and, while not dangerous on
    its own, can lead to knowledge about all of the network's participants. The
    node database could be shared with malicious nodes that could harness that
    information for attacks on the network.

  - There is no message size limit at the moment. A node will read incoming data
    until the connection for a single signal times out.

- Malicious sub-networks

  - Nodes accepting all edge requests they encounter can build up a connection
    to a large portion of the network. This opens the door for new attacks:

    - Filtering: Certain signals could be thrown away, for example edge requests
      from a certain IP range so that no new nodes can connect to the network

    - Altering signals

  - A test where two neighbours compare some of their neighbours and drop common
    ones could battle the possibility of nodes gathering up too many connections
    illegally.

  - *Immune:* Bootstrap takeover: The bootstrap server remembers all nodes it
    has helped spawn. Should the malicious network be able to bully out all
    legally known nodes, the bootstrap server would have to send the next
    request through the malicious network first. However, the newly spawned node
    now exists in the bootstrap server's database, providing a way through the
    blockade.

- Malicious swarms - right now it's trivial to spawn thousands of new nodes
  simultaneously. No matter how many honest nodes there are, it is very easy to
  drown them in a network controlled by a few actors behaving like they are
  many.

- Killing all bootstrap servers makes it impossible to discover the network.





Documentation
-------------



### Client structure

The picture below sketches the flow of information in a single Amœba client.

- Network connections are shown in red. Nodes first connect to another node's
  server (dashed red), which then relays them to their own private worker in the
  target node (by spawning a new worker, yellow dashed), at which point the data
  flows directly from node to worker.

- Workers take input from upstream nodes and formulate a response based on them.
  This response is then sent over the internal channels (blue) to the clients.

- Clients have persistent connections to downstream neighbours open (red network
  connections), and send the instructions received from the channels to them.

- The client pool watches internal databases to determine whether there are
  enough workers and clients. If not, it instructs existing clients to send
  requests for further neighbours.

![(Picture missing, uh oh)](doc/information_flow.png
                            "Flow of information in an Amœba client")



### The protocol

The protocol type used by Amœba can be found in `src/Types/Signal.hs`. All
signals are sent downstream, with one exception where relevant data actually
flows upstream. Unless otherwise noted, the server answers signals with a
`ServerSignal`, which can basically be `OK` or one of multiple possible errors.
A usual request consists of a node sending a signal downstream and waiting for
the response, terminating the worker if it is not positive.

Signals are divided in two main groups, normal and special. Normal signals are
what usual nodes routinely use:

- `EdgeRequest` contains information for establishing a new edge in the network
- `KeepAlive` is sent in case there haven't been any useful signals, but the
  connection should not time out
- `ShuttingDown` is sent as a courtesy to other nodes, so they can remove a
  terminating node before the timeout kicks in
- `Flood` signals are distributed to every node in the network. Current
  instances are text messages and a request to send a `NeighbourList` to a
  drawing server.
- `Prune` is a request to downstream neighbours to drop the connection. This is
  an effort to keep the number of connections as low as possible, and the
  request will be accepted if dropping the connection does not cross the minimum
  threshold.

Normal signals are filtered: only when they're coming from known upstream nodes
they are processed. Special signals circumvent this, as some processes
inherently require unknown nodes to establish connections.

- `BootstrapRequest` is sent to the bootstrap server, and instructs it to send
  out `EdgeRequest`s on behalf of the contacting node.
- `Handshake` is what actually establishes a new connection. Sent to a new
  downstream neighbour, it adds the issuer to its list of known nodes and
  answers with `OK`; the issuer then does its own bookkeeping, and answers back
  `OK` as well, finalizing the deal with mutual agreement.
- `HandshakeRequest`s prompt another node to send back a `Handshake`. This
  allows `Handshake` to be used to establish incoming connections, not just
  outgoing ones by sending it directly.
- `NeighbourList` is sent from nodes to the drawing server. It contains the list
  of downstream neighbours.



### Terminology, abbreviations

These may help reading the source comments:

        Name | Meaning
-----------: | -----------------------------------------------------------------
          _* | Accessor functions that don't do any computation otherwise.
          *H | Handler. Signals or commands are delegated to these for processing.
         BSS | Bootstrap server
         DSN | Downstream node, i.e. a neighbouring node the current sends commands do. (S, T, U in the picture above.)
         LDC | Local direct connection. Used by the node pool to send signals directly to its nodes instead of taking a detour over the network.
        ST1C | Server to one (unspecified/arbitrary) client channel
         STC | Server to client channel
        STSC | Server to specific client channel
         USN | Upstream node, i.e. a neighbouring node the current gets commands sent by. (A, B, C in the picture above.)

Sometimes, I like to use capital letters at the end of identifiers to tag
functions with a purpose. This is usually local to a single module. If you see
suspiciously looking names like `fooX` or `barL`, have a look at the module's
head comment.
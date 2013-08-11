Amoeba
======

Amoeba is a program for setting up a decentralized network. The name comes from the hope that eventually, the network will be so robust that you can poke any of its parts without endangering its overall integrity.

**Amoeba is currently in the phase that comes before Alpha (coding, compiling, but not running it because it's full of placeholders).**

Planned features
----------------

- Every node only knows about its immediate neighbours. Unless explicitly added, the origin of a signal sent over the network is untraceable.
- Every subprocess has its own thread, using message passing to communicate.
- The network should be robust against bad data input, e.g. from a client that sends nonsense (or even wrong) requests.

Research goals
--------------

- How many neighbours does each node need so the network isn't clustered/destroyed even when large numbers of nodes go offline?

- How large of a network can a single RasPi handle? :-)

- How long does a new message need to reach the entire network?

- What does the network look like after some time?

- How can certain attacks on the network be prevented?

    - A small number of modified nodes accept every edge request, i.e. every time a node tries to connect to the network, the malicious nodes accept it (instead of possibly relaying it). These malicious nodes will eventually gather up connections to a *lot* of nodes. For a certain (but small) number of such modified nodes, the network integrity could be endangered, e.g. when they filter new edge requests.

    - Rubbish data: How much is the network affected by spamming it with nonsense edge requests?


Terminology
-----------

- _foo: Accessor functions that don't do any computation otherwise
- DSN:  Downstream node, i.e. a neighbouring node the current sends commands do.
- ST1C: Server to one (unspecified/arbitrary) client channel
- STC:  Server to client channel
- STSC: Server to specific client channel
- USN:  Upstream node, i.e. a neighbouring node the current gets commands sent by.
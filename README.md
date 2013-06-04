Botnet
======

Botnet is a program for setting up a decentralized network, inspired by Bitcoin or BitTorrent. Its purpose is being a practice problem; I am starting this without any prior knowledge of graphs or sockets, out of a certain interest in whether I could create something similar. (While the name may be suggestive, it makes no attempt at doing anything illegal.)

Planned features
----------------

- Every node only knows about its immediate neighbours. Unless explicitly added, the origin of a signal sent over the network is untraceable.
- Every subprocess has its own thread, using STM channels to communicate. This results in a lock-free and highly concurrent environment.
- The network should be robust against bad data input, e.g. from a client that sends nonsense (or even wrong) requests.

Research goals
--------------

- How many neighbours does each node need so the network isn't clustered/destroyed even when large numbers of nodes go offline?
- How large of a network can a single RasPi handle? :-)
- How long does a new message need to reach the entire network?
- What does the network look like after some time?
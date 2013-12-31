Git branching model
===================

- **stable**:  Reasonably stable versions without obvious or neck-breaking bugs.
               The **stable** branch contains releases, and no development
               happens on this branch. All commits on it are merges of
               **master**.
- **master**:  "Nightly build" level of stability; merges **develop** branches,
               and stable commits can be merged into **stable**. Although no
               source-level development happens here, commits may contain edits
               of meta files (such as the readme or build options).
- **develop**: Main branch for development. Often contains broken or extremely
               buggy builds, excessive debug messages, hacks etc.
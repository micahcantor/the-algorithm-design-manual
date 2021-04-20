## Exercise 2-44

We have 1,000 data items to store on 1,000 nodes. Each node can store copies of exactly three different items. Propose a replication scheme to minimize data loss as nodes fail. What is the expected number of data entries that get lost when three random nodes fail?

### Replication Scheme
Let `items` be the set of items [0, 1, ..., 1000]. Let `nodes` be the set of nodes [0, 1, ..., 1000]. Let i be the ith node. The first slot in each node should be `items[i]`. The second slot should be taken by `items[i+1]` where the last item rotates to the position of the first node. The third slot should be taken by `items[i+2]` where the elements of items are rotated again. With this replication schema, Eah node will have three distinct items, and each item will appear in three different nodes.

### Expected number of data loss
When three random nodes fail, it is overwhelmingly likely that all three nodes will not contain one particular datum spread across the three respective spots. This would occur with probability 3/1000, or .3%. Therefore we expect 0 data entries are lost in the average case.
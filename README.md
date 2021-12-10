(3) We could use a stack-based finite state machine as computation model. The top of the stack contain the current state. 
It can pop itself from the stack and push new state. Lets say we have 2 states SENT and RECEIVED, if the current state is SENT on the top then we can loop here until the transition happens (blocking). 
Spawning a task can be interpreted as create another FSM.

(5) briefly explain Bitcoin's UTXO model of transaction validation (separate from POW)

In Bitcoin, each transaction spends output from prior transactions and generates new outputs that can be spent by transactions in the future. All of the unspent transactions are kept in each fully-synchronized node, and therefore this model is named “UTXO”. A user’s wallet keeps track of a list of unspent transactions associated with all addresses owned by the user, and the balance of the wallet is calculated as the sum of those unspent transactions.

https://medium.com/@sunflora98/utxo-vs-account-balance-model-5e6470f4e0cf

(6) what is the structure of a Block in bitcoin and how does it relate to the 'blockchain' (merkle tree vs merkle list of merkle trees)

The structure of a block includes hash of block k, hash of block k-1 and trasaction of representation which is a merkle tree. The blockchain will keep a list of these all blocks.

https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=8746079

(7) what problem/s are POW/POS trying to solve? discuss/compare (byzantine fault tolerance, reaching a single consensus on a p2p network)

BFT discusses about how a distributed system can survive when a byzantine fault happens. For example, how the system handles the case of 50% of nodes failure, new nodes join/upgrade, and more. This leads the development of consensus protocols, which discuss about some algorithms like traffic leader selection, Follow-the-satoshi, commitee, and cryptographic sortition, etc.
**This module is for the RPC-client. For the core module, refer to https://github.com/bitcoin-s/bitcoin-s-core.**

##Prerequisites:
* Scala: http://www.scala-lang.org/
* SBT: http://www.scala-sbt.org/download.html
* Bitcoin-Core: https://bitcoin.org/en/download

##Getting Started

In a terminal, start bitcoin testnet, and enter an SBT console:
```scala
tom@tom:~/dev/bitcoin-s-rpc-client$ bitcoind -testnet -daemon
Bitcoin server starting

tom@tom:~/dev/bitcoin-s-rpc-client$ sbt console
[info] Loading project definition from /home/tom/dev/bitcoin-s-rpc-client/project
[info] Set current project to bitcoin-s-rpc-client (in build file:/home/tom/dev/bitcoin-s-rpc-client/)
[info] Starting scala interpreter...
[info] 
Welcome to Scala version 2.11.4 (OpenJDK 64-Bit Server VM, Java 1.8.0_66-internal).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import org.bitcoins.rpc.ScalaRPCClient
import org.bitcoins.rpc.ScalaRPCClient

scala> val test = new ScalaRPCClient("bitcoin-cli","-testnet")
test: org.bitcoins.rpc.ScalaRPCClient = org.bitcoins.rpc.ScalaRPCClient@32e933d8
```

We can use the `sendCommand` function to make RPC calls in the SBT console:
```scala
scala> test.sendCommand("getblockcount")
res0: String =
"808463
"

scala> test.sendCommand("getblockchaininfo")
res1: String =
"{
    "chain" : "test",
    "blocks" : 808464,
    "headers" : 808464,
    "bestblockhash" : "0000000000d42f628e1e3b9754c0b245a08a9998378c7d40ee31838a347531f7",
    "difficulty" : 1.00000000,
    "verificationprogress" : 1.00000230,
    "chainwork" : "00000000000000000000000000000000000000000000000a745080437a62ecc9"
}
"

```

Bitcoin-S has some RPCs that can be called with functions. Some return a single value:
```scala
scala> test.getBlockCount
res2: Int = 808463
```

Others return interactive JSON objects:
```scala
scala> test.getBlockChainInfo
res3: org.bitcoins.rpc.bitcoincore.blockchain.BlockchainInfo = BlockChainInfoImpl(test,808464,808464,000000000041d1047d0f2d8415c518baf367574d9d2583e491873c1bdf6f5a79,1.0,1.00000230,00000000000000000000000000000000000000000000000a745080437a62ecc9)

scala> test.getBlockChainInfo.chain
res4: String = test

scala> test.getBlockChainInfo.chainWork
res5: String = 00000000000000000000000000000000000000000000000a745080437a62ecc9

scala> test.getBlockChainInfo.bestBlockHash
res6: String = 0000000000d42f628e1e3b9754c0b245a08a9998378c7d40ee31838a347531f7

```

Not all RPCs are written into functions as of yet.

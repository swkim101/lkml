# LKML

```haskell
main = do
  handle <- openFile "test/single.mbox" ReadMode
  contents <- System.IO.hGetContents handle
  let mbox = parse contents

  dumpTree mbox -- [(<message id>, Maybe <message id>), ...]
  dumpMap mbox -- [(<message id>, <message content>), ...]
```
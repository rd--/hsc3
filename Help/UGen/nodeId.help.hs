-- nodeId ; except that negative nodeId are not reported
sinOsc AR (mce2 (-100) (nodeID IR * 100)) 0 * 0.1

---- ; specify node id
audition_at (-1,AddToHead,1,[]) (sinOsc AR (mce2 (-100) (nodeID IR * 100)) 0 * 0.1)
audition_at (8,AddToHead,1,[]) (sinOsc AR (mce2 (-100) (nodeID IR * 100)) 0 * 0.1)

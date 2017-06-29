# PITcleanr

> > Clean up PIT tag capture histories for use by DABOM

Our purpose with this package is to take a valid tag list from an adult fish trap, query [PTAGIS](https://www.ptagis.org) to get a complete capture history. From there, we will match up detections with identified nodes in DABOM, then transform the long data to a wide format, using those nodes as columns. In the process of doing so, we'll flag tags that show a complex detection history, meaning they were detected at multiple branches on their upstream journey. 

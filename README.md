# pplacer suite of programs

[![wercker status](https://app.wercker.com/status/b3fe580ff3a62d0a69c6da264c03ba81/s/master "wercker status")](https://app.wercker.com/project/bykey/b3fe580ff3a62d0a69c6da264c03ba81)

`pplacer` places reads on a phylogenetic tree.
`guppy` (Grand Unified Phylogenetic Placement Yanalyzer) yanalyzes them.
`rppr` is a helpful tool for working with reference packages.

* [project webpage (with compiled binaries)](http://matsen.fhcrc.org/pplacer/)
* [documentation](http://matsen.github.com/pplacer/)
* [tutorial](http://fhcrc.github.com/microbiome-demo/)
* [compilation instructions](http://matsen.github.com/pplacer/compiling.html)

`pplacer`, `guppy`, and `rppr` are free software under the GPL v3.

+Several other tools have used pplacer as one of their main components. Some of these include:
+
+* **SEPP** (Mirarab, Nguyen, Warnow, PSB, 2012) aims to improve the scalability of phylogenetic placement using divide-and-conquer. It uses Ensembles of Hidden Markov Models (implemented by HMMER) to both align sequnces and to find a (small) subtree for placement using pplacer. [SEPP](github.com/smirarab/sepp) can place the green genes dataset with 200,000 sequences. A standalone version for the greengenes reference is available [here](https://github.com/smirarab/sepp/wiki/SEPP-on-Greengenes).

+* **paprica** (Bowman and Ducklow, PLoS One, 2015) uses Infernal and pplacer to place reads on a reference tree of 16S rRNA genes from all completed genomes in Genbank.  The domains Bacteria, Archaea, and Eukarya are all supported.  [paprica](https://github.com/bowmanjeffs/paprica) normalizes for 16S rRNA gene copy number and provides and estimate of the enzymes, metabolic contents, and genomic character (e.g. GC content, genome length) of the community.  [paprica](https://github.com/bowmanjeffs/paprica) is available as an Amazon Machine Instance or VirtualBox appliance, however, we recommend that you install the dependencies and run locally.  A basic tutorial can be found [here](http://www.polarmicrobes.org/analysis-with-paprica/).

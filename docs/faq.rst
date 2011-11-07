.. faq_:

FAQ
===

How can I cite pplacer?
-----------------------
Right now there is only one paper to cite::

  @article{matsen2010pplacer,
    title={pplacer: linear time maximum-likelihood and Bayesian phylogenetic placement of sequences onto a fixed reference tree},
    author={Matsen, F.A. and Kodner, R.B. and Armbrust, E.},
    journal={BMC Bioinformatics},
    volume={11},
    number={1},
    pages={538},
    year={2010},
    publisher={BioMed Central Ltd}
  }

At some point we will write something up for the new pplacer, guppy, taxtastic, etc., but right now the focus is on the software.


Why is pplacer taking up so much memory?
----------------------------------------
Because it caches likelihood vectors for all of the internal nodes.
This is what makes it fast!

Pplacer uses about 1/4 of the memory when placing on a FastTree tree as compared to a RAxML tree inferred with GTRGAMMA.
If your reads are short and in a fixed region, the memory used by pplacer v1.1 alpha08 (or later) scales with respect to the total number of non-gap columns in your query alignment.
You can also make it use less memory (and run faster) by cutting down the size of your reference tree.

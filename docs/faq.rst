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


Can I run pplacer with a FastTree tree?
---------------------------------------
Not right now, because pplacer uses the Gamma model of rate heterogeneity.
We plan to allow the CAT model (what FastTree uses), but that's an optimistic several months away.


Why is pplacer taking up so much memory?
----------------------------------------
Because it caches likelihood vectors for all of the internal nodes.
This is what makes it fast!

However, you can make it use less memory (and run faster) by cutting down the size of your reference tree.
Also, if your reads are short and in a fixed region, the memory used by pplacer v1.1 alpha08 (or later) scales with respect to the total number of non-gap columns in your query alignment.

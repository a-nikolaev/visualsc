
# Simplicial complex visualization

**visualsc** is a tool similar to [Graphviz](http://graphviz.org/) designed for visualizing simplicial complexes rather than graphs. It is using physics-like simulation for choosing good node layout.

**Some examples:**

[![screenshot](http://i.imgur.com/Fg1vOCJ.png)](http://i.imgur.com/pV0MvNy.png)

![screenshot](http://i.imgur.com/dkFtylh.png)

**Input format:**   
Simply plain text listing all facets of a simplicial complex. Each facet should be enclosed in curly braces `{...}` The nodes can be  arbitrary integers separated by spaces, commas (or actually anything other than the numbers or curly braces).
		
	{1 2 3 4 5} {1 5 6 7} {2 8 9} {7 8}

New-line and other whitespace characters can be added for clarity. In fact, it makes sense to improve the program allowing arbitrary strings as nodes. 

 ![](http://i.imgur.com/hf4kVvF.png) 

## Mathematical definition
A **Simplicial Complex** [[see an article in Wikipedia](https://en.wikipedia.org/wiki/Abstract_simplicial_complex)] is a family of non-empty finite sets closed under the operation of taking non-empty subsets.

For example, consider a simplicial complex **SC = { {1, 2, 3}, {1, 4}, {1, 2}, {1, 3}, {2, 3}, {1}, {2}, {3}, {4} }**. If **x** is an element of **SC**, then all non-empty subsets of **x** are also in **SC**. 

The elements of a simplicial complex are called **faces**. (There are 9 faces in this example SC). 

Since all non-empty subsets of a face are automatically in the simplicial complex, the complex can be fully described by listing only those faces that are not subsets of any other bigger face. Such **maximal faces are called facets**.

The example **SC** has only two facets: **{1, 2, 3}** and **{1, 4}**.


# How to build and run

You have to install `ocaml` compiler with `cairo2` package for rendering PDFs. It is recommended to use OCaml package manager `opam` to install `cairo2`.

To build:

	make

Example usage:

    ./visualsc -i data.sc -o image.pdf
    ./visualsc < data.sc > image.pdf

Please see built-in help for more options (choosing PRNG seed, raw output, etc.) 

    ./visualsc -h 

# License

The software is distributed under the conditions of the BSD 3-clause license.


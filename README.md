
# Simplicial complex visualization

**visualsc** is a simple command-line tool similar to [Graphviz](http://graphviz.org/) designed for visualizing 
abstract simplicial complexes rather than graphs. It is using physics-like spring-forces simulation for choosing good node layout. 
Output is a PDF file. You may use other tools such as ImageMagick to convert to PNG, SVG, or other formats.

**Some examples:**

![screenshot](http://i.imgur.com/WRx6JVB.png)

![screenshot](http://i.imgur.com/05iR0Fm.png)

**Input format:**   
The input format is a plain text file listing all facets of the simplicial complex. 
Each facet is a set of nodes enclosed in curly braces `{...}`. For example:
		
	{1, 2, 3, 4, 5} {1, 5, 6, 7} {2, 8, 9} {7, 8}
	
  Or it's enough to separate nodes with spaces:
  
	{1 2 3 4 5} {1 5 6 7} {2 8 9} {7 8}

The nodes can be arbitrary integers that are separated by spaces, commas, semicolons
(or actually anything other than the numbers or curly braces).
New-line and other whitespace characters can be added for clarity. 
In fact, it makes sense to improve the program allowing arbitrary strings as nodes. 

If the input simplicial complex is stored the file `input.sc`, then we can run the program:

	./visualsc -i input.sc -o output.pdf

The output PDF will look as follows:

 ![](http://i.imgur.com/qN8Ct65.png) 

## Mathematical definition
An **abstract simplicial complex** \[[see the article in Wikipedia](https://en.wikipedia.org/wiki/Abstract_simplicial_complex)\]
is a family of non-empty finite sets closed under the operation of taking non-empty subsets.

For example, consider a simplicial complex **SC = { {1, 2, 3}, {1, 4}, {1, 2}, {1, 3}, {2, 3}, {1}, {2}, {3}, {4} }**. If **x** is an element of **SC**, then all non-empty subsets of **x** are also in **SC**. 

The elements of a simplicial complex are called **faces**. (There are 9 faces in this example SC). 

Since all non-empty subsets of a face are automatically in the simplicial complex, the complex can be fully described by listing only those faces that are not subsets of any other bigger face. Such **maximal faces are called facets**.

The example **SC** has only two facets: **{1, 2, 3}** and **{1, 4}**.


# How to build and run

You have to install `ocaml` compiler with `cairo2` package for rendering PDFs. It is recommended to use OCaml package manager `opam` to install `cairo2`:
  
	opam install cairo2

To build:

	make

Example usage:

	./visualsc -i input.sc -o output.pdf
	./visualsc < input.sc > output.pdf

Please see built-in help for more options (choosing PRNG seed, reading and writing raw coordinates output, etc.) 

	./visualsc -h 

# Advanced features

## Seed

The `seed` of the PRNG can be specified as follows:

	 ./visualsc --seed=12345 ...
     
## "Raw" output and input

If you only want to compute **the coordinates of each node** without making a PDF file,
you can request a `raw` text output:

	 ./visualsc --raw ...

The above feature can be useful if you have a different rendering engine, for example,
so you only need to compute the layout of the nodes, and your other tool will handle the drawing part.

The raw output format is simple text, one line per node that specifies its coordinates:

      node  X  Y
      
For example, for a complex with 9 nodes, the output can look like:

      1  1.98296   1.58422
      2  1.41239   1.13481
      3  1.14466   2.35119
      4  0.864804  1.71882
      5  1.91916   2.3105
      6  2.8368    2.34959
      7  2.8212    1.37355
      8  2.28669   0.348882
      9  1.27763   0.0544191

The raw coordinates output can be also read from a file as the **initial** positions of the nodes:

	 ./visualsc -I complex.coords ...

One use case for reading and writing raw coordinates is when you want to draw two similar complexes `1.sc` and `2.sc`.
How to make both complexes look comparable? You can:

      ./visualsc --raw -i 1.sc -o 1.coords
      ./visualsc -I 1.coords -i 1.sc -o 1.pdf
      ./visualsc -I 1.coords -i 2.sc -o 2.pdf

We first computed the coordinates of `1.sc` and saved them in the file `1.coords`.
The we use those coordinates as an initial input for rendering both complexes, so the same nodes will be close in both images.
This trick will also work if you need a sequence of complexes rendered as a progression of snapshots, for example, here we do it for three complexes:

![](http://i.imgur.com/BDX7YlW.png)

You can use the tools such as `pdfunite` and `pdfjam` to join multiple PDFs into one file or one page. 
Graphical vector editors such as Inkscape can be also helpful if you need extra image manipulation.

# License

The software is distributed under the conditions of the BSD 3-clause license.


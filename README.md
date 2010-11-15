# linear2tree: Tiny web based linguistic utility to convert linear syntax representation into 2D syntax tree represenation

This is a small linguistics typesetting utility to help eople who write
linguistics papers and are in need of typesetting parse trees.

I myself was one of those people and tried to draw trees using drawing software
which took much time and did not produce high quality output. However, later I
realized that a linear representation of a parse tree such as:

    [PP [Spec right] [P' [P across] [NP [Spec the] [N' [N bridge]]]]]

can be automatically translated to the traditional two dimensional tree form and
included in a paper.

So I wrote a small program using Common Lisp programming language hoping it can
help the linguistic research community and ease their job writing and
typesetting beautiful papers.

The current shortcomings of program:

* It doesn't produce .pdf output.</li>
* It is tested only for Turkish and English alphabet.
* The Turkish character set is only handled correctly for .png output. (due to some PostScript font problems).
* There are no ways to adjust the size and other parameters of the output.

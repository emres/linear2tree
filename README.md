# linear2tree: Tiny web based linguistic utility to convert linear syntax representation into 2D syntax tree represenation

**Warning:** The source code is about 5 years old and is the result of my
experiments with Common Lisp to help fellow linguists. I did not test it with
current Lisp implementations, web servers, etc. but probably it will either work
out of the box provided that you have an active SBCL system and allegroserve or
will require minor modifications. And probably by now somebody should have
created a more capable utility to solve the same problem (let me
know). Nevertheless I decided to share it so that it may prove useful for some
linguists and hackers out there.

This is a small linguistics typesetting utility to help eople who write
linguistics papers and are in need of typesetting parse trees.

I myself was one of those people and tried to draw trees using drawing software
which took much time and did not produce high quality output. However, later I
realized that a linear representation of a parse tree such as:

    [PP [Spec right] [P' [P across] [NP [Spec the] [N' [N bridge]]]]]

can be automatically translated to the traditional two dimensional tree form and
included in a paper such as:

![linear2tree example output](http://farm2.static.flickr.com/1426/5178878879_e0a32632c0.jpg "linear2tree example output")

So I wrote a small program using Common Lisp programming language hoping it can
help the linguistic research community and ease their job writing and
typesetting beautiful papers.

The current shortcomings of program:

* It doesn't produce .pdf output.
* It is tested only for Turkish and English alphabet.
* The Turkish character set is only handled correctly for .png output. (due to some PostScript font problems).
* There are no ways to adjust the size and other parameters of the output.

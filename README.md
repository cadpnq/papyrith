# Papyrith
A Papyrus compiler written in Common Lisp.

Papyrith is a papyrus compiler that aims to produce better code than the ones that came before it. It does this by having a peephole optimizer and doing some basic data flow analysis. You can find some output examples for the different compilers [here.](https://github.com/cadpnq/papyrith/wiki/Comparison)

This project is very much a work in progress and is not currently suitable for real use. There are many parts of the language that are not yet implemented.

There are some papyrith channels on the [ASPr discord](https://discord.gg/X6Udqfx) if you're interested in snooping on the development of this project.

## FAQ
**Q:** Can I use papyrith to keep people from decompiling my code?

**A1:** Probably. Thanks to the aggressiveness of the optimization pass, champollion will most likely not understand any nontrivial bit of code that papyrith puts out.

**A2:** (the real answer) **No!** Short of writing horrible spaghetti assembly by hand there is nothing you can do to keep people from taking your code apart. Even then you're only making it more difficult. This applies to every papyrus compiler.
___

**Q:** Can I use the papyrith optimizer on code produced by another compiler?

**A:** Not currently, but this is a planned feature. Most of the optimization happens the assembly level so it will just be a matter of writing some code to parse `.pex`/`.pas` files.

___

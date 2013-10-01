# Interactive Assembly Mode #

Inspired by Justine Tunney's disaster.el (http://github.com/jart/disaster‎).

iasm provides a simple interactive interface objdump and ldd which
facilitates assembly exploration. It also provides tools to speed up the
edit-compile-disasm loop.

This mode currently only supports Linux because it relies rather heavily on
objdump and ldd. It also hasn't been tested for other CPU architectures or
other unixes so expect some of the regexes to spaz out in colourful ways.

Note that this is my first foray into elisp so monstrosities abound. Go forth
at your own peril. If you wish to slay the beasts that lurk within or simply
add a few functionalities, contributions are more then welcome. See the todo
section for ideas.


# Installation #

Make sure to place `iasm-mode.el` somewhere in the load-path and add the
following lines to your `.emacs` to enable iasm:

```lisp
(require 'iasm-mode)

(global-set-key (kbd "C-c C-d") 'iasm-disasm)
(global-set-key (kbd "C-c C-l") 'iasm-ldd)

(add-hook 'c-mode-common-hook
          (lambda ()
           (local-set-key (kbd "C-c d") 'iasm-goto-disasm-buffer)
           (local-set-key (kbd "C-c l") 'iasm-disasm-link-buffer)))
```

# iasm-mode #

iasm mode can be invoked using the `iasm-disasm` function which will prompt
for an object file to disassemble. While you can provide just about anything
that objdump supports, the mode currently only processes the .text section.
iasm-disasm will then open up a new buffer with the symbols for that object
file.

Additionally, when working on source files, `iasm-disasm-link-buffer` will
invoke `iasm-disasm` on the given object file and will link the current
buffer with the newly opened iasm buffer. This then allows you to invoke
`iasm-goto-disasm-buffer` to quickly jump back to the iasm buffer and refresh
it if the object file was modified. This is useful to quickly test the effect
of change.

By default, all symbols are hidden which is a good thing. This makes it
easier to search for a specific symbol and it allows iasm to lazily load the
symbols assembly which is important when dealing with large object files. To
toggle the visibility of a symbol simply move to the appriate like and hit
`TAB` which will prompt iasm to either retrieve the relevant assembly from
objdump or show/hide the symbol's assembly. `c` can be used to hide all the
sections and `M-n` `M-p` can be used to jump to the next/previous section.

When moving around instructions, `s` will open a the source file at the line
associated with that instruction. Alternatively, you can change line with `n`
and `p` which will automatically track the source file. `C-n` and `C-p` work
as usual. When on a jump instruction, `j` will jump to the target address if
available. Note that this may trigger a symbol to be loaded.

Finally, `g` can be used to refresh the buffer if the object file was
modified and `q` will close the buffer.


# ldd-mode #

`iasm-ldd-mode` is simple front-end for ldd which dumps all the dynamic
libraries that a object file depends on. This can be used in conjunction with
`iasm-disasm` to quickly locate symbols that aren't in the current object
file.

To create a open ldd buffer, either invoke the `iasm-ldd` function or press
`l` in an iasm buffer. In the resulting buffer, you can then press `d` to
invoke `iasm-disasm` on a the given library. Hit `j` or `RET` to invoke
`iasm-ldd` on the target library.

Finally, `g` can be used to refresh the buffer if the object file was
modified and `q` will close the buffer.


# Todos #

- Shorten the edit-compile-disasm loop
  - Introduce compilation into the loop somehow.

- Static analyses
  - Highlight all uses of a register.
    - Could even go as far as trace the entire use graph.
  - basic-block detection (highlight and loop detection would be nice).
  - Show jump edges (basic-block highlighting should work).

- Improvements:
  - Write tests... Just kidding! Well not really, index could use some love.
  - Should probably scope all the disasm specific stuff to iasm-disasm. That
    change touches almost half the functions in this mode so it'll wait.

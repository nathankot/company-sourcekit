# company-sourcekit

Completion for Swift projects via SourceKit with the help of
[SourceKitten][sourcekitten].

<img src="https://raw.githubusercontent.com/nathankot/company-sourcekit/master/screen.gif" width="384" height="296" />

### Work in progress

Please see [this issue](https://github.com/nathankot/company-sourcekit/issues/2)
for the remaining tasks that need to be completed. Any help would be awesome d(^.^)b

Usual contribution guidelines apply: branch, formatting (`indent-region`) etc.

### Installation

Currently this package works without project-specific completion. I.e it can
only intelligent about `Foundation` modules and the current file.

To install, just `git clone https://github.com/nathankot/company-sourcekit` into
your [load path](http://www.emacswiki.org/emacs/LoadPath) and `(require
'company-sourcekit)`.

As of _2015-11-13_, [SourceKitten][sourcekitten] needs to be built from master
in order for this to work due to
[this](https://github.com/jpsim/SourceKitten/issues/97) yet-to-be-released
patch.

### License

> Copyright (c) 2015 Nathan Kot
> 
> Permission is hereby granted, free of charge, to any person obtaining a copy
> of this software and associated documentation files (the "Software"), to deal
> in the Software without restriction, including without limitation the rights
> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
> copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:
> 
> The above copyright notice and this permission notice shall be included in
> all copies or substantial portions of the Software.
> 
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
> THE SOFTWARE.

[sourcekitten]: https://github.com/jpsim/SourceKitten

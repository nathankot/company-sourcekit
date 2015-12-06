# company-sourcekit

_[company-mode][company] completion for Swift projects via SourceKit with the help of [SourceKittenDaemon][sourcekittendaemon]._

<img src="https://raw.githubusercontent.com/nathankot/company-sourcekit/master/cap.gif" width="384" height="281" />

#### Limitations

* Only works when there is a `*.xcodeproj` up the directory tree.
* OSX-only, since this communicates with SourceKit which only runs on OSX (AFAIK)
* You need to have `curl` on your machine

#### Installation

First install [SourceKittenDaemon][sourcekittendaemon]. Once you have it
installed on your `PATH` and emac's `exec-path` install company-sourcekit by
adding it to your `load-path` and then:

```elisp
(require 'company-sourcekit)
```

_MELPA release is coming soon._

#### Configuration

* _`company-sourcekit-use-yasnippet`_ - Use yasnippet for completion expansion. By default this is enabled if yasnippet is detected.
* _`company-sourcekit-verbose`_ - Log company-related messages to `*messages*` with verbosity
* _`sourcekit-available-ports`_ - A list of ports that `sourcekittendaemon` is allowed to listen on.
* _`sourcekit-sourcekittendaemon-executable`_ - Location of the `sourcekittendaemon` executable
* _`sourcekit-curl-executable`_ - Location of the `curl` executable
* _`sourcekit-verbose`_ - Log sourcekittendaemon-related messages to `*messages*` with verbosity

#### How it works

* company-sourcekit communicates with sourcekittendaemon via HTTP.
* Which in turn communicates with SourceKit using the [sourcekitten][sourcekitten] framework.
* sourcekittendaemon will read your `.xcodeproj` file and determine the best configuration options to pass to sourcekit.

#### Credits

* Big thanks to [terhechte][terhechte] for spearheading SourceKittenDaemon!

[terhechte]: https://github.com/terhechte
[company]: https://github.com/company-mode/company-mode
[sourcekittendaemon]: https://github.com/terhechte/SourceKittenDaemon
[sourcekitten]: https://github.com/jpsim/SourceKitten

#### License

See [`LICENSE`](https://github.com/nathankot/company-sourcekit/blob/master/LICENSE).

# Arizona Website

[![Netlify Status](https://api.netlify.com/api/v1/badges/f52c6501-93d0-4298-91b9-4d8297b48823/deploy-status)](https://app.netlify.com/projects/arizona-framework/deploys)

The official website for the Arizona Framework - a modern Erlang web framework
for building real-time web applications.

## Overview

A static page generated with Arizona itself: the views in `src/` are rendered once, at build time,
by `arizona_static`, into `dist/`, which Netlify serves. The counter shown on the page is
`examples/counter.erl`, compiled with the site, so the sample always builds.

## Development

Requires Erlang/OTP 29+, rebar3 and Node.js (for the Tailwind CSS build).

```bash
make dist      # build the CSS and the page into dist/
make preview   # serve dist/ at http://localhost:8000
```

Commit the rebuilt `dist/` with your change: it is what gets deployed.

## Project structure

```
src/
├── arizona_website.erl             # build/0: generates dist/
├── arizona_website_home_page.erl   # the page
└── arizona_website_layout.erl      # the HTML shell
examples/
└── counter.erl                     # the sample shown on the page
priv/static/                        # assets, images and icons, copied into dist/
```

## License

Copyright (c) 2025 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.

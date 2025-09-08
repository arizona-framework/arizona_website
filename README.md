# Arizona Website

[![Netlify Status](https://api.netlify.com/api/v1/badges/f52c6501-93d0-4298-91b9-4d8297b48823/deploy-status)](https://app.netlify.com/projects/arizonaframework/deploys)

The official website for the Arizona Framework - a modern Erlang web framework
for building real-time web applications.

## Overview

This project showcases the Arizona Framework's capabilities through an
interactive website featuring:

- **Real-time interactivity** with WebSocket-based updates
- **Component-based architecture** with reusable UI components
- **Static site generation** for deployment
- **Responsive design** with Tailwind CSS
- **Syntax highlighting** for code examples
- **Scroll animations** and modern UI effects

## Development

### Prerequisites

- Erlang/OTP 27+
- Rebar3
- Node.js (for CSS build)

### Setup

```bash
# Clone the repository
git clone git@github.com:arizona-framework/arizona_website.git
cd arizona_website

# Install dependencies
npm install
rebar3 deps

# Compile the project
rebar3 compile

# Start the development server
rebar3 shell
```

The website will be available at http://localhost:1912

### CSS Development

```bash
# Build CSS (development)
npm run build:css
```

## Static Site Generation

Generate a static version of the website:

```bash
rebar3 shell --eval "arizona_website_static:generate(), init:stop()."
```

The static site will be generated in the `dist/` directory.

## Project Structure

```
src/
├── arizona_website_app.erl          # Application entry point
├── arizona_website_conf.erl         # Server configuration
├── arizona_website_home_page.erl    # Homepage component
├── arizona_website_components.erl   # Reusable UI components
├── arizona_website_layout.erl       # HTML layout template
├── arizona_website_static.erl       # Static site generator
└── arizona_website_view.erl         # Main view controller

priv/static/
├── assets/                          # JavaScript and CSS files
├── images/                          # Arizona Framework logos
└── favicon.ico                      # Site favicon
```

## License

Copyright (c) 2025 [William Fank Thomé](https://github.com/williamthome)

Arizona is 100% open-source and community-driven. All components are
available under the Apache 2 License on [GitHub](https://github.com/arizona-framework/arizona).

See [LICENSE.md](LICENSE.md) for more information.

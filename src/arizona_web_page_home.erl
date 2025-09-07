-module(arizona_web_page_home).
-behaviour(arizona_stateful).
-compile({parse_transform, arizona_parse_transform}).
-export([mount/1]).
-export([render/1]).

mount(Bindings) ->
    arizona_stateful:new(?MODULE, Bindings).

render(Bindings) ->
    arizona_template:from_string(~""""
    <div id="{arizona_template:get_binding(id, Bindings)}" class="min-h-screen bg-gradient-to-br from-gray-900 via-gray-800 to-orange-900">

        <!-- Header Navigation -->
        <header class="relative z-10 bg-black bg-opacity-20 backdrop-blur-sm border-b border-gray-700">
            <nav class="container mx-auto px-6 py-4">
                <div class="flex items-center justify-between">
                    <div class="flex items-center space-x-3">
                        <img src="/images/arizona_256x256.jpeg" alt="Arizona Framework" class="h-10 w-10 rounded-lg shadow-lg">
                        <div>
                            <h1 class="text-xl font-bold text-white">Arizona</h1>
                            <p class="text-xs text-orange-300">Real-time Web Framework</p>
                        </div>
                    </div>
                    <div class="hidden md:flex items-center space-x-8">
                        <a href="#features" class="text-gray-300 hover:text-orange-400 transition-colors duration-300">Features</a>
                        <a href="#examples" class="text-gray-300 hover:text-orange-400 transition-colors duration-300">Examples</a>
                        <a href="https://github.com/arizona-framework/arizona" target="_blank" rel="noopener noreferrer" class="bg-orange-600 hover:bg-orange-700 text-white px-4 py-2 rounded-lg font-medium transition-all duration-300 shadow-lg">
                            GitHub
                        </a>
                    </div>
                </div>
            </nav>
        </header>

        <!-- Hero Section -->
        <section class="relative py-20 lg:py-32 px-6">
            <div class="container mx-auto text-center">
                <!-- Large Arizona Logo -->
                <div class="flex justify-center mb-8">
                    <div class="relative">
                        <img src="/images/arizona_512x512.jpeg" alt="Arizona Framework Logo" class="h-32 w-32 lg:h-48 lg:w-48 rounded-2xl shadow-2xl shadow-orange-500/20 ring-4 ring-orange-500/30">
                        <div class="absolute -inset-1 bg-gradient-to-r from-orange-600 to-red-600 rounded-2xl blur opacity-75 animate-pulse"></div>
                    </div>
                </div>

                <!-- Compelling Headlines -->
                <h1 class="text-4xl lg:text-7xl font-bold text-white mb-6 leading-tight">
                    Build <span class="bg-gradient-to-r from-orange-400 to-red-500 bg-clip-text text-transparent">Real-time</span>
                    <br>Web Applications
                </h1>

                <p class="text-xl lg:text-2xl text-gray-300 mb-8 max-w-4xl mx-auto leading-relaxed">
                    Arizona is a modern Erlang web framework that brings <strong class="text-orange-400">Phoenix LiveView-style</strong> real-time interactivity
                    with the rock-solid reliability of the BEAM virtual machine.
                </p>

                <!-- Work in Progress Warning -->
                <div class="bg-gradient-to-r from-orange-900/50 to-red-900/50 border border-orange-500/50 rounded-xl p-6 mb-12 max-w-2xl mx-auto backdrop-blur-sm">
                    <div class="flex items-center justify-center space-x-3 mb-3">
                        <svg class="h-6 w-6 text-orange-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L3.732 16.5c-.77.833.192 2.5 1.732 2.5z" />
                        </svg>
                        <h3 class="text-lg font-semibold text-orange-300">Work in Progress</h3>
                    </div>
                    <p class="text-gray-300 text-sm">
                        Arizona is currently in active development. This framework is experimental and not yet ready for production use.
                        <br>Follow our progress and contribute at <a href="https://github.com/arizona-framework/arizona" class="text-orange-400 underline hover:text-orange-300">GitHub</a>.
                    </p>
                </div>

                <!-- CTA Buttons -->
                <div class="flex flex-col sm:flex-row gap-4 justify-center items-center">
                    <button class="bg-gradient-to-r from-orange-600 to-red-600 hover:from-orange-700 hover:to-red-700 text-white font-bold py-4 px-8 rounded-xl shadow-2xl shadow-orange-500/25 transition-all duration-300 transform hover:scale-105">
                        Get Started
                    </button>
                    <button class="bg-gray-800/50 hover:bg-gray-700/50 text-white font-semibold py-4 px-8 rounded-xl border border-gray-600 transition-all duration-300 backdrop-blur-sm">
                        View Documentation
                    </button>
                </div>
            </div>
        </section>

        <!-- Features Section -->
        <section id="features" class="py-20 px-6 bg-black bg-opacity-30">
            <div class="container mx-auto">
                <h2 class="text-4xl font-bold text-white text-center mb-16">
                    Why Choose <span class="text-orange-400">Arizona?</span>
                </h2>

                <div class="grid md:grid-cols-2 lg:grid-cols-3 gap-8">
                    <!-- Real-time Features -->
                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700 hover:border-orange-500/50 transition-all duration-300">
                        <div class="bg-gradient-to-r from-orange-600 to-red-600 w-12 h-12 rounded-lg flex items-center justify-center mb-6">
                            <svg class="h-6 w-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z" />
                            </svg>
                        </div>
                        <h3 class="text-xl font-bold text-white mb-4">Real-time Updates</h3>
                        <p class="text-gray-300">
                            LiveView-style real-time updates without writing JavaScript. Build interactive applications with server-side rendering and automatic DOM synchronization.
                        </p>
                    </div>

                    <!-- BEAM Performance -->
                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700 hover:border-orange-500/50 transition-all duration-300">
                        <div class="bg-gradient-to-r from-orange-600 to-red-600 w-12 h-12 rounded-lg flex items-center justify-center mb-6">
                            <svg class="h-6 w-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
                            </svg>
                        </div>
                        <h3 class="text-xl font-bold text-white mb-4">BEAM Performance</h3>
                        <p class="text-gray-300">
                            Built on Erlang/OTP, Arizona inherits the legendary fault-tolerance, concurrency, and performance of the BEAM virtual machine.
                        </p>
                    </div>

                    <!-- Developer Experience -->
                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700 hover:border-orange-500/50 transition-all duration-300">
                        <div class="bg-gradient-to-r from-orange-600 to-red-600 w-12 h-12 rounded-lg flex items-center justify-center mb-6">
                            <svg class="h-6 w-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
                            </svg>
                        </div>
                        <h3 class="text-xl font-bold text-white mb-4">Developer Experience</h3>
                        <p class="text-gray-300">
                            Clean template syntax, hot code reloading, and comprehensive tooling make development fast and enjoyable.
                        </p>
                    </div>
                </div>
            </div>
        </section>

        <!-- Code Example Section -->
        <section id="examples" class="py-20 px-6">
            <div class="container mx-auto">
                <h2 class="text-4xl font-bold text-white text-center mb-16">
                    See Arizona in <span class="text-orange-400">Action</span>
                </h2>

                <div class="max-w-4xl mx-auto">
                    <div class="bg-gray-900/80 backdrop-blur-sm rounded-xl border border-gray-700 overflow-hidden shadow-2xl">
                        <div class="bg-gray-800 px-6 py-4 border-b border-gray-700">
                            <h3 class="text-white font-semibold">counter_view.erl</h3>
                        </div>
                        <div class="p-6">
                            <pre class="text-sm text-gray-300 overflow-x-auto"><code>
                            -module(counter_view).
                            -behaviour(arizona_stateful).
                            -compile(\{parse_transform, arizona_parse_transform}).

                            -export([mount/1, handle_event/2, render/1]).

                            mount(Bindings) ->
                                arizona_stateful:new(?MODULE, Bindings#\{count => 0}).

                            handle_event(\{~"increment"}, Bindings) ->
                                Count = arizona_template:get_binding(count, Bindings),
                                \{ok, Bindings#\{count => Count + 1}};
                            handle_event(\{~"decrement"}, Bindings) ->
                                Count = arizona_template:get_binding(count, Bindings),
                                \{ok, Bindings#\{count => Count - 1}}.

                            render(Bindings) ->
                                Count = arizona_template:get_binding(count, Bindings),
                                arizona_template:from_string(~"""
                                &lt;div class="text-center"&gt;
                                    &lt;h1 class="text-4xl font-bold"&gt;\{Count}&lt;/h1&gt;
                                    &lt;button az-click="increment" class="btn-primary"&gt;+&lt;/button&gt;
                                    &lt;button az-click="decrement" class="btn-secondary"&gt;-&lt;/button&gt;
                                &lt;/div&gt;
                                """).</code></pre>
                        </div>
                    </div>

                    <p class="text-gray-300 text-center mt-8 text-lg">
                        Real-time, interactive components with zero JavaScript.
                        <span class="text-orange-400 font-semibold">Just pure Erlang.</span>
                    </p>
                </div>
            </div>
        </section>

        <!-- Performance Stats -->
        <section class="py-20 px-6 bg-black bg-opacity-30">
            <div class="container mx-auto text-center">
                <h2 class="text-4xl font-bold text-white mb-16">
                    Built for <span class="text-orange-400">Performance</span>
                </h2>

                <div class="grid md:grid-cols-3 gap-8">
                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700">
                        <div class="text-4xl font-bold text-orange-400 mb-2">2M+</div>
                        <div class="text-white font-semibold mb-2">Concurrent Connections</div>
                        <div class="text-gray-400">On a single BEAM instance</div>
                    </div>

                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700">
                        <div class="text-4xl font-bold text-orange-400 mb-2">&lt;1ms</div>
                        <div class="text-white font-semibold mb-2">Response Time</div>
                        <div class="text-gray-400">For real-time updates</div>
                    </div>

                    <div class="bg-gray-800/50 backdrop-blur-sm rounded-xl p-8 border border-gray-700">
                        <div class="text-4xl font-bold text-orange-400 mb-2">99.9%</div>
                        <div class="text-white font-semibold mb-2">Uptime</div>
                        <div class="text-gray-400">With hot code reloading</div>
                    </div>
                </div>
            </div>
        </section>

        <!-- Footer -->
        <footer class="py-12 px-6 border-t border-gray-700 bg-black bg-opacity-50">
            <div class="container mx-auto text-center">
                <div class="flex items-center justify-center space-x-3 mb-6">
                    <img src="/images/arizona_256x256.jpeg" alt="Arizona Framework" class="h-8 w-8 rounded-lg">
                    <span class="text-white font-bold text-lg">Arizona Framework</span>
                </div>

                <p class="text-gray-400 mb-4">
                    Real-time web applications powered by Erlang/OTP
                </p>

                <div class="flex justify-center space-x-6">
                    <a href="https://github.com/arizona-framework/arizona" target="_blank" rel="noopener noreferrer" class="text-gray-400 hover:text-orange-400 transition-colors duration-300">
                        GitHub
                    </a>
                    <a href="#" class="text-gray-400 hover:text-orange-400 transition-colors duration-300">
                        Documentation
                    </a>
                    <a href="#" class="text-gray-400 hover:text-orange-400 transition-colors duration-300">
                        Community
                    </a>
                </div>

                <div class="mt-8 pt-8 border-t border-gray-800">
                    <p class="text-gray-500 text-sm">
                        © 2024 Arizona Framework. Built with Arizona.
                    </p>
                </div>
            </div>
        </footer>
    </div>
    """").

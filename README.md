# Dialog

Declarative, code-driven user interfaces and applications in F#. Inspired by [React](http://facebook.github.io/react/).

License will be BSD.

## Motivation

Conceptually, Dialog is new in many respects:

- First, and most importantly, Dialog introduces an almost pure functional component system, which is conceptually simpler to understand than lenses or monads. Though not implemented as such, the component system works like a [term rewriting system](http://en.wikipedia.org/wiki/Rewriting).
- Dialog allows user interfaces to be specified in a declarative way. A specification looks similar to the content that is presented on the screen.
- Dialog introduces an internal DSL, which is comparable to other external DSLs/XML grammars for specifying user interfaces. F#'s list comprehensions seemed to be a great fit and turned out to be more readable and more flexible than XML, or React's JSX for that matter.
- Dialog uses statically typed properties and passes them around in dynamically typed lists `obj list`. This combines the flexibility of dynamically typed programming languages with the strength of statically typed ones.
- Dialog's component model supports multiple hierarchies of independent subtrees of functionality. For example a nested component may present a popover at any time by rendering it somewhere in its view hierarchy. The popover does not affect the primary view hierarchy, but gets picked up by an independent service, that scans over the component hierarchy and shows the popover when needed. This extensibility mechanism may be capable to support the specification of the complete application state, not only its user interface.

## Examples

- [Interaction.fs](https://github.com/pragmatrix/Dialog/blob/master/Dialog.Tests/InteractionTests.fs)   
![](screenshots/interaction.png)
- [StandardControls.fs](https://github.com/pragmatrix/Dialog/blob/master/Dialog.Tests/StandardControls.fs)   
![](screenshots/standard-controls.png)
- [Popover.fs](https://github.com/pragmatrix/Dialog/blob/master/Dialog.Tests/PopoverTests.fs)

## Vision

- One, free, community driven, user interface framework for all platforms. Mobile first.
- Dialog's declarative component system is suited towards live programming. One idea is to use [NCrunch](http://www.ncrunch.net/), an automated testing framework, with [Nessos.Vagabond](http://nessos.github.io/Vagabond/), a library that supports the dynamic distribution of .NET code, to create a tool that supports state preserving and live user interface previews while typing.
- It may be possible, that Dialog components can be completely decoupled from the user interface implementation and run on the thread pool. The consequence is, that component rendering can automatically be distributed on the available cores.

## Status

- Layout is based on a C# port of [Facebook's css layout](https://www.github.com/pragmatrix/css-layout), which is also available as a [NuGet package](https://www.nuget.org/packages/Facebook.CSSLayout/).
- The API is not yet stable, naming changes and conceptual redesigns will follow.
- A bunch of iOS controls are implemented, not much more.
- The project compiles in Visual Studio + Xamarin.iOS, but not yet in Xamarin Studio, because of a [bug](https://bugzilla.xamarin.com/show_bug.cgi?id=27744).
- Windows apps, and Windows Phone apps can not be supported yet. It seems that the FSharp compiler can not handle windows runtime events properly. To implement the native wrappers on these platforms, F# needs to support Profile32 PCLs.
- For faster prototyping, a WPF implementation is planned.

## Roadmap 1.0

- Support most iOS controls, including scroll and list view containers.
- Try to make the component tree immutable and distribute the render calls to the thread pool.
- Async support for actor-like components, support timers, and state changes that are triggered be triggered by external events.
- Be sure the API is stable and internal types and functions are protected properly.
- Cache the component's render output.

## And then

- Support Windows Apps, Windows Phone, and Android.
- Add components that specify application and navigation states.

# fs2-d3

A pure-FP take on [D3.js](https://github.com/d3/d3) for [Scala.js](https://www.scala-js.org/)
built on top of [Cats Effect](https://typelevel.org/cats-effect/) and [FS2](https://fs2.io/).

Check out the [microsite](https://buntec.github.io/fs2-d2/).

Disclaimer: This library is experimental, so use at your own risk! No artifacts are published yet.

## Getting started

Have a look at the examples subproject. You can run the examples by doing `sbt examples/fastLinkJS` and then serving
`index.html` from the `examples` folder using something like [Live Server](https://www.npmjs.com/package/live-server).

## APIs covered
 - d3-selection: mostly
 - d3-transition: mostly
 - d3-ease: partly
 - d3-interpolate: partly
 - d3-color: partly

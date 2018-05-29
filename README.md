# SOP

core: [ ![Download](https://api.bintray.com/packages/bigknife/maven/core/images/download.svg) ](https://bintray.com/bigknife/maven/core/_latestVersion)

effect: [ ![Download](https://api.bintray.com/packages/bigknife/maven/effect/images/download.svg) ](https://bintray.com/bigknife/maven/effect/_latestVersion)

> Sequential computation over parallel computation

It's a simplified tool for building pure FP programs in scala.

It comes from [Freestyle](https://github.com/frees-io/freestyle), but `Freestyle` is complicated, I only need some basic
elements for building pure functional programs.

## Concepts

### Par
`Par` is the abstraction of parallel 


## Memo

We should add a sbt plugin of `partial-unification` to avoid `SI-2712`:

```
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.0.0")
```

@Millesabin has fix it in 2.12: [https://github.com/scala/scala/pull/5102](https://github.com/scala/scala/pull/5102)

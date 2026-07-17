<div align="center">

# 🏰 Immutable Towers

**Tower Defense game built in Haskell** — project for *Laboratórios de Informática I* @ University of Minho

![Haskell](https://img.shields.io/badge/-Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)
![Cabal](https://img.shields.io/badge/-Cabal-6E4A7E?style=for-the-badge&logo=haskell&logoColor=white)
![HUnit](https://img.shields.io/badge/-HUnit-orange?style=for-the-badge)

</div>

---

## 🚀 Getting Started

### Build & Run

Compile and run the project using Cabal:

```bash
cabal run --verbose=0
```

### Interpreter

Open the Haskell interpreter (GHCi) with the project loaded:

```bash
cabal repl
```

---

## 🧪 Testing

The project uses [HUnit](https://hackage.haskell.org/package/HUnit) for unit testing.

Run the test suite:

```bash
cabal test
```

Generate a test coverage report:

```bash
cabal test --enable-coverage
```

Documentation examples can also be run as tests via [`doctest`](https://hackage.haskell.org/package/doctest):

```bash
cabal install doctest
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

---

## 📚 Documentation

Project documentation is generated with [Haddock](https://haskell-haddock.readthedocs.io/):

```bash
cabal haddock
```

---

<div align="center">

🔗 [github.com/miguelprcorreia/Immutable-Towers](https://github.com/miguelprcorreia/Immutable-Towers)

</div>

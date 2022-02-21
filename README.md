# OOX
OOX comes in two parts: the language and a verification tool for this language. The following sections will describe each part separately.

## The OOX Language

---

OOX is an intermediate verification language (IVL) with objects and concurrency as first class citizens. The syntax of OOX will be familiar to those experienced with object-oriented languages as e.g. Java and C#.

For example, see the OOX program below. In this program, we have a counter that incremented the field `current` in the class `Counter` from its initial value `0` to `4` in a concurrent fashion.

```none
class Counter {
    int current;

    Counter(int initial) {
        this.current := initial;
    }

    void increment() {
        lock(this) {
            int value := this.count;
            this.current := value + 1;
        }
    }

    static void count(int initial, int N)
            requires(initial >= 0)
            exceptional(false) {
        Counter counter := new Counter(initial);
        int i := 0;
        while(i < N) {
            fork counter.increment();
            i := i + 1;
        }
        join;
        int value := counter.current;
        assertvalue == initial + N;
    }

    static void main(int initial) {
        count(initial, 4);
    }
}
```

For those interested, OOX also has a [formal semantics](https://dspace.library.uu.nl/bitstream/handle/1874/396688/thesis.pdf?sequence=1).

## The OOX Verification Tool

---

The OOX language comes with a tool to verify such programs with a tool also named OOX. It is based on symbolic execution and supports all language features as defined in the OOX language.

### Installation

OOX is developed with GHC, and is tested using GHC 8.8.4. It also requires the SMT solver [Z3](https://github.com/Z3Prover/z3), specifically version 4.8.8.

Simply use the command `cabal v2-build` to build or `cabal v2-repl` to interactively use OOX. Cabal will ensure that the required Haskell packages are installed. The [Z3 package](https://hackage.haskell.org/package/z3) will require a bit more work, as it requires the Z3 bindings. On Windows, the fields `extra-include-dirs` and `extra-lib-dirs` can be added to the Cabal configuration file located in the user-specific AppData.

### Usage

The OOX verification tool can be tweaked using the following set of parameters.

| Description | Long | Short | Mandatory | Example |
| -           | -    | -     | -         | -       |
| The file to verify |            |       | V         | "program.oox" |
| The entry function | `--function` | `-f`    | V | `-f "Program.main` |
| Maximum verification depth | `--path-depth` | `-k` |  | `-k 150` |
| Ignore the post-condtions | `--no-ensures` | | | `--no-ensures` |
| Ignore the exceptional post-condtions | `--no-ensures` | | | `--no-exceptional` |
| Ignore the pre-condtions | `--no-ensures` | | | `--no-requires` |
| Do not consider symbolic `null`  values | `--no-symbolic-null` | | | `--no-symbolic-null` |
| Do not consider symbolic  aliases | `--no-symbolic-alias` | | | `--no-symbolic-alias` |
| The maximum symbolic array size to consider | `--symbolic-array-size` | | | `--symbolic-array-size 3` |
| Disable formula caching | `--no-cache` | | | `--no-cache` |
| Disable partial order reduction | `--no-por` | | | `--no-por` |
| Disable local evaluation | `--no-local-solver` | | | `--no-local-solver` |
| Disable random exploration | `--no-random-interleaving` | | | `--no-random-interleaving` |
| The logging level | `--inform` | `-i` | | `-i 1` |

For example, the command `oox "program.oox" -f "Program.main" -k 150 -i 0` verifies the function `main` in the class `Program` in the program `program.oox` up to a maximum depth of `150` with logging disabled.

### Paper

Stefan Koppier, [_The Path Explosion Problem in Symbolic Execution, an Approach to the Effects of Concurrency and Aliasing_](./doc/koppier_thesis.pdf). Master thesis, Utrecht University, 2020.

# The Miva Manifesto

> **Explicit. Safe. Predictable.**

---

## 1. What is Miva?

Miva is more than an acronym—it is a promise:

**Memory‑safe, Intuitive, Verifiable, Adaptable**  
These four pillars define Miva.

---

## 2. What are the characteristics of Miva?

- **Memory‑safe**  
  Eliminates use‑after‑free, double‑free, and data races at compile time, **with no garbage collection**.

- **Verifiable behavior**  
  All operations are expressed explicitly—no implicit copies, no magic conversions, no runtime surprises.

- **Predictable execution**  
  Zero runtime overhead, no GC pauses, no dynamic dispatch.
  **Performance like C, safety like Rust.**

---

## 3. Core features of Miva

### 1. **Clean ownership system**

- `move x`: Explicitly transfers ownership; the original variable becomes invalid immediately
- `clone x`: Explicit deep copy with clear, unambiguous semantics
- `ref x: T`: Used only for function parameters to denote a read‑only borrow
  (**not a type**, does not escape the function)

> Ownership rules:
>
> - **Copy types** (e.g., `int`, `bool`, pure‑value structs) → copied automatically
> - **non‑Copy types** (e.g., `[T]`, `string`) → require either `move` or `clone`

### 2. **Expression‑oriented syntax**

```miva
// C-style
int foo() { return 1; }

// Miva-style
foo = (): int => 1
```

- Functions are first‑class values: assignable, passable, returnable
- Single‑expression functions omit `{}` and `return`
- Multi‑statement functions automatically return the **last expression**

### 3. **Modern error handling**

- Uses lightweight algebraic type `Result[T, E]`
- **Errors must be handled explicitly**; ignoring return values is forbidden
- Deconstruct `Result` via the `choose` statement, ensuring all branches are covered
- Supports `?` syntactic sugar, similar to Rust

```miva
res := choose may_fail() {
    when Ok(value) { value }
    when Err(e) {
        log("Error: ", e)
        return -1
    }
};
res := may_fail()?;
```

- `panic` is reserved for unrecoverable errors (e.g., out‑of‑memory)
  and is **uncatchable and unrecoverable**

### 4. **Clear pattern matching: the `choose` statement**

```miva
choose value {
    when 0 { "zero" }
    when 1 { "one" }
    when x if x > 10 { "big number" }
    otherwise { "other" }
};
```

- No `fallthrough` pitfalls
- No redundant symbols (compared to Rust’s `match` or C’s `switch`)
- Supports guard conditions (`if`) and future deconstruction capabilities

---

## 4. What Miva explicitly rejects

| Feature | Reason |
|---------|--------|
| **Garbage Collection (GC)** | Introduces unpredictable latency, violating the “predictable” principle |
| **`panic` catching mechanism** | `panic` represents program failure and should not be used as control flow |
| **`match` / `switch`** | `match` uses verbose syntax; `switch` has counterintuitive default fallthrough |
| **`try`/`catch`/`finally`** | Exception jumps break linear code flow; explicit error handling is encouraged |
| **`goto` or arbitrary labels** | Breaks structured programming; Miva supports only natural control flow |
| **Global mutable variables** | Introduces concurrent data race issues |

> What Miva *does not* do matters more than what it does—
> restraint is the highest form of design.

---

## 5. Miva feature overview

### 5.1 Unsafe model

- Miva’s FFI requires explicit `unsafe` declaration; calls take the form `unsafe ffi_function( /* params */ )`
- Any function that calls an `unsafe` function must itself be marked `unsafe`
- If a function calls `unsafe` code but the author guarantees **memory safety**, it may be marked `trusted`
- Functions marked `trusted` or `unsafe` require annotation comments: `@trusted: ...` or `@unsafe: ...`
- A function not marked `unsafe` may not call `unsafe` functions, but may call `trusted` functions
- This model is more explicit and safer than Rust’s `unsafe`

### 5.2 FFI model

- Miva supports two kinds of FFI: external and inline
- `import "c:....h"` imports header files, which are **translated to actual `include` directives**
- After `import`, all functions reside under the `c` module and are `unsafe`
- Example:

```miva
import "c:stdio.h"; // may be written c:cstdio but not recommended

// Safe function
foo = () => {
  c.printf("Hello, foo!"); // invalid
}

unsafe main = () => {
 c.printf("Hello"); // valid
}
```

- Miva also allows inline **C functions**
- Example:

```miva
import "c:stdio.h"

c unsafe foo = (ref inp1: int, inp2: int) => { // c unsafe required
 printf("Hello, %d %d", inp1, inp2); // direct C function calls
 // no c.xxx prefix needed
 // may call other functions in the same file, but not other modules
 // may access parameters directly
 // C++ code is allowed but strongly discouraged:
 std::string a = "Hello";
 printf("Hello, %s", a.c_str()); // not recommended
}
```

- Miva does not verify the safety of any C or FFI code

### 5.3 Concurrency model

- Miva uses `async/await` as its concurrency model
- Syntax:

```miva
async foo = () => {
 ...
}

main = () => {
 res1 := foo(); // res1 is future<unit>
 res2 := await foo(); // res2 is unit
}
```

- Internally implemented using **C++20 coroutines**

### 5.4 Programming style

- Miva follows a **DOP + FP** style
- Rejects full OOP, but provides `obj.method()` syntactic sugar
- All Miva functions should be **free functions**
- **No function overloading**
- Supports **operator overloading**:

```miva
MyStruct = struct {
  a: int
}
add_mystruct(a: MyStruct, b: MyStruct) => {
  struct MyStruct {
    a = (a.a + b.a)
  }
}
impl MyStruct {
  operator_add = add_mystruct
}
```

- Naming conventions:
  1. **Structs**: `PascalCase`
  2. **Variables / functions / filenames**: `snake_case`
  3. **Modules**: lowercase `foo.bar.xx`
- No global variables:
  - **Immutable globals** are replaced by functions: `global = (): type => val`
  - **Mutable global variables** are a major source of race conditions; Miva strictly forbids them

### 5.5 Pointers

- Miva provides **two pointer types**
- `ptr<T>`: raw pointer
  - **Taking the address of a variable** is safe: `addr var`
  - **Dereferencing a `ptr<T>`** is unsafe: `deref ptr // only in unsafe or trusted functions`
- `box<T>`: heap pointer
  - Has **unique ownership**, prohibits shallow copies, and is **automatically freed at end of lifetime**
  - **Dereferencing a `box<T>`** is safe: `deref box`

---

## 6. The Miva Oath

> Trust the programmer,
> but do not test their memory.
> One less frowning symbol,
> one more intuitive simplicity.
> Aim not for omnipotence,
> but for:
> expressing the safest semantics
> with the cleanest syntax,
> generating the most efficient code.
> Miva is not a simplified Rust,
> nor a safer Go.
> Miva is Miva:
> a language built for the future.

---

## 📜 License

MIT License. Use it, break it, improve it.

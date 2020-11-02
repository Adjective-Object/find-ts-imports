# find-ts-imports

Small package to find the imports of a typescript source string without leaving rust.

Because there is no canonical e/BNF grammer for typescript, this relies on regex search for a quick & dirty way to locate import statements, require expressions, and webpack import() calls. This approach is likely a bit too permissive. You have been warned.


```sh
cargo watch -x test
```

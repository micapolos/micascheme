import:
  from: leo3, define
  from: check
define: hello: "Hello"
define: world: "world"
define:
  exclamate: string
  string append: string, "!"
check: is equal: hello, "Hello"
check: is equal: world, "world"
check: is equal:
  exclamate: "foo"
  "foo!"

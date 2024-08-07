This project uses scala-cli (scala-cli.virtuslab.org)

Usage exmaples -

format the project

```
 scala-cli fmt
```

run tests
```
scala-cli test .
```

running the compare on one of the test files 

```
GITHUB_OUTPUT="output.txt" INPUT_FILES="resources/V8.scala_test"  scala-cli run .
```

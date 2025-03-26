e: edit
edit:
	vi src/main/scala/Borg.scala src/main/scala/BorgMemory.scala src/test/scala/BorgTest.scala
c: compile
compile:
	sbt compile
t: test
test:
	sbt test
.PHONY: c compile e edit t test

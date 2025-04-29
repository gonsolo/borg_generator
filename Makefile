e: edit
edit:
	vi src/main/scala/*.scala src/test/scala/*.scala
c: compile
compile:
	sbt compile
t: test
test:
	sbt test
.PHONY: c compile e edit t test

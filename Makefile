SCALAHOME:=/opt/scala-2.10.0-M6/bin

all: package

compile:
	@mkdir -p bin
	@echo Compiling...
	@$(SCALAHOME)/scalac -language:implicitConversions -feature -usejavacp -unchecked -deprecation -d bin src/*.scala

doc:
	@mkdir -p doc
	@echo Generating API documentation...
	@$(SCALAHOME)/scaladoc -usejavacp -unchecked -deprecation -d doc src/*.scala

package: compile
	@echo Packaging rapture-io.jar
	@jar cmf etc/manifest rapture-io.jar -C bin rapture

clean:
	@rm -fr bin
	@rm -f rapture-io.jar
	@echo Cleaned bin and rapture-io.jar

int:
	@$(SCALAHOME)/scala -cp rapture-io.jar

.PHONY: compile package clean

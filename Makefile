BUILD_DIR=$(shell stack --docker path --local-install-root)

build:
	stack build --docker
	cp ${BUILD_DIR}/bin/aws-lambda-haskell-runtime-client-exe bootstrap
	zip function.zip bootstrap
	rm bootstrap

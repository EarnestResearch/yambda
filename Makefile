# @todo: add the last path only when in travis
export PATH := ${PATH}:/usr/local/bin:/usr/bin:/bin:/home/travis/build/EarnestResearch/aws-lambda-haskell-runtime-client/.stack-work/docker/_home/.local/bin

BUILD_DIR = build
EXAMPLES  = api-gateway kinesis s3

default: stylish sam-tests

build: lint ## build linux version using docker
	stack build --test --copy-bins --local-bin-path ${BUILD_DIR}

dev: clean ## build continuously
	stack build --no-docker --file-watch

devt: clean ## build/test continuously
	stack test --no-docker --file-watch

test: clean ## test
	stack test --no-docker

clean: ## clean
	stack clean

lint: ## lint examples src
	hlint examples src

lint-test: ## lint test
	hlint test

sam-tests: sam-template-all package-all ## sam test
	sam local generate-event apigateway aws-proxy | \
		sam local invoke "APIGatewayEcho" -t build/api-gateway.yaml
	sam local generate-event kinesis get-records | \
		sam local invoke "KinesisEcho" -t build/kinesis.yaml
	sam local generate-event s3 delete | \
		sam local invoke "S3Echo" -t build/s3.yaml
	sam local generate-event s3 put | \
		sam local invoke "S3Echo" -t build/s3.yaml

package-all: build ## package all in examples
	$(foreach EXAMPLE,$(EXAMPLES),NAME=$(EXAMPLE) $(MAKE) package;)

package: ## package custom lambda: bootstrap
	cp ${BUILD_DIR}/${NAME}-exe ${BUILD_DIR}/bootstrap
	cd ${BUILD_DIR} && zip ${NAME}.zip bootstrap && rm bootstrap

upload: sam-template ## updload sam package
	sam package \
		--template-file ${BUILD_DIR}/${NAME}.yaml \
		--s3-bucket ${S3_BUCKET} \
		--output-template-file ${BUILD_DIR}/${NAME}-S3.yaml

sam-template-all: ## gen all sam templates for examples
	$(foreach EXAMPLE,$(EXAMPLES),NAME=$(EXAMPLE) $(MAKE) sam-template;)

sam-template: ## gen sam template
	sed -e "s/{{ZIP_FILE_PATH}}/${NAME}.zip/g" examples/${NAME}/template.yaml > ${BUILD_DIR}/${NAME}.yaml
	sam validate -t ${BUILD_DIR}/${NAME}.yaml

deploy: upload ## deploy
	sam deploy \
		--template-file ${BUILD_DIR}/${NAME}-S3.yaml \
		--stack-name aws-lambda-haskell-runtime-${NAME} \
		--capabilities CAPABILITY_IAM

stylish: ## sytlish haskell
	find src -name '*.hs' | xargs stylish-haskell -i

docker-pull: ## pull docker image aws-lambda-haskell-platform
	stack docker pull

PACKAGES = hlint stylish-haskell
stack-install: ## stack install utilities
	for i in $(PACKAGES); do stack install $$i;  done

help: ## help
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

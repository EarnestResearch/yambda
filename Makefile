BUILD_DIR = build
EXAMPLES  = api-gateway kinesis s3

.PHONY: build
build: ## compile and test
	stack build --docker --test --copy-bins --local-bin-path ${BUILD_DIR}

sam-tests: sam-template-all package-all ## compile, test, and invoke each example with a generated event
	sam local generate-event apigateway aws-proxy | \
		sam local invoke "APIGatewayEcho" -t build/api-gateway.yaml
	sam local generate-event kinesis get-records | \
		sam local invoke "KinesisEcho" -t build/kinesis.yaml
	sam local generate-event s3 delete | \
		sam local invoke "S3Echo" -t build/s3.yaml
	sam local generate-event s3 put | \
		sam local invoke "S3Echo" -t build/s3.yaml

.PHONY: package-all
package-all: build
	$(foreach EXAMPLE,$(EXAMPLES),NAME=$(EXAMPLE) $(MAKE) package;)

.PHONY: package
package:
	cp ${BUILD_DIR}/${NAME}-exe ${BUILD_DIR}/bootstrap
	cd ${BUILD_DIR} && zip ${NAME}.zip bootstrap && rm bootstrap

.PHONY: upload
upload: sam-template ## upload lambda to S3 using SAM CLI
	sam package \
		--template-file ${BUILD_DIR}/${NAME}.yaml \
		--s3-bucket ${S3_BUCKET} \
		--output-template-file ${BUILD_DIR}/${NAME}-S3.yaml

.PHONY: sam-template-all
sam-template-all:
	$(foreach EXAMPLE,$(EXAMPLES),NAME=$(EXAMPLE) $(MAKE) sam-template;)

.PHONY: sam-template
sam-template:
	sed -e "s/{{ZIP_FILE_PATH}}/${NAME}.zip/g" examples/${NAME}/template.yaml > ${BUILD_DIR}/${NAME}.yaml
	sam validate -t ${BUILD_DIR}/${NAME}.yaml

.PHONY: deploy
deploy: upload ## deploy lambda to AWS using SAM CLI
	sam deploy \
		--template-file ${BUILD_DIR}/${NAME}-S3.yaml \
		--stack-name aws-lambda-haskell-runtime-${NAME} \
		--capabilities CAPABILITY_IAM

.PHONY: stylish
stylish: ## run stylish haskell code formatter
	find src -name '*.hs' | xargs stylish-haskell -i

PACKAGES = stylish-haskell
.PHONY: ready
ready: ## pull the docker image used to build, install code formatter, etc
	stack docker pull
	for i in $(PACKAGES); do stack install $$i --no-docker;  done

.PHONY: help
help: ## help
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'


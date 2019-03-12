BUILD_DIR=build
EXAMPLES=api-gateway kinesis s3

.PHONY: default
default: stylish sam-tests

.PHONY: build
build:
	stack build --docker --test --copy-bins --local-bin-path ${BUILD_DIR}

sam-tests: sam-template-all package-all
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
upload: sam-template
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
deploy: upload
	sam deploy \
		--template-file ${BUILD_DIR}/${NAME}-S3.yaml \
		--stack-name aws-lambda-haskell-runtime-${NAME} \
		--capabilities CAPABILITY_IAM

.PHONY: stylish
stylish:
	find src -name '*.hs' | xargs stylish-haskell -i


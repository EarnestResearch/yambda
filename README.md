[![Build Status](https://travis-ci.org/EarnestResearch/aws-lambda-haskell-runtime-client.svg?branch=master)](https://travis-ci.org/EarnestResearch/aws-lambda-haskell-runtime-client)

# aws-lambda-haskell-runtime-client

A custom runtime client for AWS Lamda written in Haskell.

Examples of how to use this client can be found in the `examples` directory.

## Creating a lambda based on this library
To create a lambda that is based on this library, using `stack`:

1. Add it to your extra-deps this repository
```yaml
extra-deps:
  - github: EarnestResearch/aws-lambda-haskell-runtime-client
    commit: {commit-sha}
```
2. Add it to your `package.yaml`

3. Make sure you build your lambda with static linking:
```yaml
# Put this in either package.yaml or your cabal file
# You should put it behind a flag if you need to build
# it in systems where static linking is not fully supported (like osx)
cc-options: -static
ld-options: -static -pthread
```

4. Build you lambda with static linking inside docker
```sh
# Example
stack build --docker --flag your-executable:static
```

## Contributing

### Prerequisites

1. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. [Docker](https://docs.docker.com/docker-for-mac/install/)
1. [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-install.html)
1. [SAM CLI](https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-sam-cli-install.html)
1. Run `make ready` to pull the docker image used to build, install the stylish haskell code formatter, etc.

### Building

The default make target will compile and test the executables defined in
`package.yaml`. You can find the produced artifacts in the `build` directory.

```
make
```

### Out of Memory Error

If you see an error message similar to
`Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)`,
especially on your first build, try increasing the memory available to your docker instance
or run with `stack build --docker -j1`.

### Deploy

The deploy make target will create the resources needed for the specified lambda.

```
NAME=api-gateway S3_BUCKET=<your s3 bucket name> make build package deploy
```

### Adding an Example

To add a new usage example:

1. Create a new directory in the `examples` directory. The name of this
   directory gets used as the executable name so choose accordingly.
1. Add a `Main.hs` to your new directory that implements your lambda function.
1. Add a SAM template that defines the resources needed by your lambda function
   in your new directory. It should be named `template.yaml`. Use `CodeUri:
'{{ZIP_FILE_PATH}}'` for the code URI attribute.
1. Define a new executable in `package.yaml`. Use the name of your directory as the executable name.
1. Add the directory name to the `EXAMPLES` variable in the `Makefile`.
1. Run `make` to verify there are no errors.


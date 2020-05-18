[![Build Status](https://travis-ci.org/EarnestResearch/yambda.svg?branch=master)](https://travis-ci.org/EarnestResearch/yambda)

# yambda

A custom runtime client for AWS Lamda written in Haskell.

Examples of how to use this client can be found in the `examples` directory.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [yambda](#yambda)
    - [Creating a lambda based on this library](#creating-a-lambda-based-on-this-library)
    - [Deriving encoders and decoders](#deriving-encoders-and-decoders)
    - [Contributing](#contributing)
        - [Prerequisites](#prerequisites)
        - [Building](#building)
        - [Out of Memory Error](#out-of-memory-error)
        - [Deploy](#deploy)
        - [Adding an Example](#adding-an-example)

<!-- markdown-toc end -->



## Creating a lambda based on this library
To create a lambda that is based on this library, using `stack`:

1. Add this repository to your extra-deps
```yaml
extra-deps:
  - github: EarnestResearch/yambda
    commit: {commit-sha}
```
2. Add it to your `package.yaml`

3. Build you lambda with dynamic linking inside docker

[LambCI](https://github.com/lambci) publishes runtime and build containers for Lambdas.
If you don't have any special requirements you can just add this to your `stack.yaml`

```yaml
docker:
  enable: false
  repo: "lambci/lambda:build-provided"
```

And build inside a docker container with

```sh
stack build --docker
```

`LambCI`'s container doesn't ship with any of the required Haskell tools but this works
because `stack` is able to "volume mount" everything it needs to compile into a "build" container.
See stack's [docker integration](https://docs.haskellstack.org/en/stable/docker_integration/) docs for more.

With this approach if you need extra libraries in your Lambda environment you'll probably need to create a [Lambda layer](https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html) and a "build" container to match.

Note that it's probably best not to enable docker in `stack.yaml` by default and use the `--docker` flag as this tends to be more compatible with existing Haskell tooling.


4. Static linking

You can try `stack`'s default build container or any other custom container and link statically:

```yaml
# Put this in either package.yaml or your cabal file
# You should put it behind a flag if you need to build
# it in systems where static linking is not fully supported (like osx)
cc-options: -static
ld-options: -static -pthread
```

Note that this approach is sensitive to the version of `glibc` you end up with, it *must* match
what's available in the Lambda runtime or you may get weird behaviors or crash.
Because of that you should probably explore some alternative setups if you are interested in static linking, here's an [old SO thread](https://stackoverflow.com/questions/3430400/linux-static-linking-is-dead).


## Deriving encoders and decoders
This lambda runtime supports other encoding/decoding formats besides JSON, for example `Dhall`.
For this reason, we use our own `LambdaEncode` and `LambdaDecode` classes.
If you just need to encode and decode values from JSON, you can use `DerivingVia` to automatically
derive our encoders and decoders.

Example
```haskell
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE DeriveAnyClass #-}

import AWS.Lambda.Encoding
import Data.Aeson
import Data.Text
import GHC.Generics

data User = User {
  name :: Text, 
  accountId :: Natural
} deriving (Generic, Show, FromJSON, ToJSON)
  deriving LambdaDecode via (LambdaFromJSON User)
  deriving LambdaEncode via (LambdaToJSON User)

```

We support out of the box also [Dhall](https://dhall-lang.org) encoding and decoding
using the `LambdaFromDhall` and `LambdaToDhall` classes.

You can create your custom encoders and decoders too! The important thing 
is that since AWS Lambda only supports JSON inputs, you pass your values
as escaped strings and unescape them in your decoder. 

See [Encoding.hs](src/AWS/Lambda/Encoding.hs) for some examples of encoders and decoders. 

## Contributing

### Prerequisites

1. [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. [Docker](https://docs.docker.com/docker-for-mac/install/)
1. [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-install.html)
1. [SAM CLI](https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/serverless-sam-cli-install.html)
1. Run `make ready` to pull the docker image used to build, install the stylish haskell code formatter, etc.

### Building

The default make target will compile and test the executables defined in
`yambda.cabal`. You can find the produced artifacts in the `build` directory.

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
1. Define a new executable in `yambda.cabal`. Use the name of your directory as the executable name.
1. Add the directory name to the `EXAMPLES` variable in the `Makefile`.
1. Run `make` to verify there are no errors.


# aws-lambda-haskell-runtime-client

A custom runtime client for AWS Lamda written in Haskell.

The main function is an example of how to use the runtime client to produce an
executable that can be used as a lambda proxy by API Gateway. It simply echos
back whatever it receives.

## Prerequisites ##

1. [Stack] (https://docs.haskellstack.org/en/stable/install_and_upgrade/)
1. [Docker] (https://docs.docker.com/docker-for-mac/install/)

### Building in a Docker Container ###

This project is built in a docker container that must be pulled from AWS ECR
first.

```
$(aws ecr get-login --region us-west-2 --registry-ids 137112412989 --no-include-email)
docker build --tag=aws-lambda-haskell-platform .
```

## Building ##

```
stack build
```


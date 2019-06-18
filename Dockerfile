FROM 137112412989.dkr.ecr.us-west-2.amazonaws.com/amazonlinux:2017.03.1.20170812@sha256:8b2b0d94a896c5f100ee60ad274bc7677e0038bc450642e6d142f503bb8ab9b8
RUN yum install gcc gmp-devel zlib-devel xz shadow-utils.x86_64 perl -y && \
  yum clean all
ARG STACK_VERSION=2.1.1
RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | \
  tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
RUN stack setup

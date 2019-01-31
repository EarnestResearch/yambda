FROM 137112412989.dkr.ecr.us-west-2.amazonaws.com/amazonlinux@sha256:8b2b0d94a896c5f100ee60ad274bc7677e0038bc450642e6d142f503bb8ab9b8
RUN yum install gcc gmp-devel zlib-devel xz shadow-utils.x86_64 -y
RUN curl -sSL https://get.haskellstack.org/ | sh

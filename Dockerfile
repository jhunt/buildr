FROM nginx AS base
FROM nginx:1

RUN apt-get update
RUN apt-get install -y sbcl

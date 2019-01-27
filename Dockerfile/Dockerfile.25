# This dockerfile is for Centaur Emacs (25)
# VERSION 1.0.0
# Author: Vincent Zhang
# Command format: Instruction [arguments / command] ..

# Base image to use
FROM ubuntu

# Maintainer:
MAINTAINER Vincent Zhang seagle0128@gmail.com

# Commands to update the image
RUN cp /etc/apt/sources.list /etc/apt/sources.list.bak
RUN sed -i "s/archive.ubuntu.com/mirrors.aliyun.com/g" /etc/apt/sources.list
RUN apt-get update
RUN apt-get upgrade -y

# Install Centaur Emacs
RUN apt-get install -y emacs-nox git
RUN git clone --depth 1 https://github.com/seagle0128/.emacs.d.git ~/.emacs.d

ENV TERM xterm-256color
WORKDIR /root

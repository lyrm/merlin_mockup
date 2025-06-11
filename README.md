(WIP)

# Purpose

The purpose of the repository is to mimick the general structure of the typer part of 
Merlin code to design, document and discuss several optimization i
- using a background domain that performs the typing computation;
- partial typing
- request cancellation mechanism

# TODO
- proper tests ! 
- better typer simulation
- Improve README

# Design Explanation

## The role of each domain
- one domain (called main domain) is handling the server's requests, which 
includes doing the analysis before answering

- one domain (caller typer domain) is computing the 'pipeline'

## Sharing data

TODO 

## Partial typing

The idea is that for some requests it mays be enough to only do a partial typing of the buffer, until the next top level definition of the current buffer for example. To do so, the 


The typer domain can return a partial result to the main domain and keep typing the buffer until the end of it or cancellation. 

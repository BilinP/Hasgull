# Changelog for `Hasgull`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.0.0 - 2025-03-22

### Added
 Helper function to deal with symbols that are bundled with identifier "a1: 5;", 
takes a string and returns a list of tokens. While that works, We need to figure out if it's needed and if so,
how to best implement it into the tokenizer logic

## Unreleased

### Changed
 - modified convertToToken logic 

## 0.1.0.0 - 2005-03-13

### Added
 - stripWhitespace Function to take input string and return a list of strings
   Example StripWhiteSpace "Hello World" -> ["Hello","World"]
 - dataypte token to handle all of our tokens. 
 - convertToToken which takes a string and converts to the equivalent token.
 - tokenize which takes a string and returns a list of tokens.
   This just takes the output of stripWhitespace and maps it with converToToken

## 0.1.0.0 - YYYY-MM-DD

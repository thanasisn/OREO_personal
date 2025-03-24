#!/usr/bin/env bash
## created on 2025-03-24

#### A function to activate OREO

function oreo() {
  echo "Starting OREO environment"
  cd "$HOME/OREO/operation"
  conda activate oreo
  setsid spyder &
  setsid rstudio &
}



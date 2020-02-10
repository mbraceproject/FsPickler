#!/usr/bin/env bash

dotnet tool restore
dotnet fake run build.fsx "$@"
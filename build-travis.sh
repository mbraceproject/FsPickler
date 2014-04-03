#!/bin/bash
if [ ! -f packages/FAKE/tools/Fake.exe ]; then
  mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi
if [ ! -f packages/SourceLink.Fake/tools/SourceLink.fsx ]; then
  mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
fi

run_fake() {
	configuration=$1
	echo "Building $1..."
	mono packages/FAKE/tools/FAKE.exe build.fsx -d:MONO Configuration="$1"
}

run_fake "Release"
run_fake "NoEmit"

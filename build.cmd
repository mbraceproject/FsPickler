@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(X86)%\MSBuild\12.0\Bin
MSBuild.exe tools/FsPickler.proj
tools\NuGet.exe pack tools\FsPickler.nuspec -outputDirectory build

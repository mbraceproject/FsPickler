@echo off

dotnet tool restore
dotnet paket restore
packages\build\FAKE\tools\FAKE.exe build.fsx %*
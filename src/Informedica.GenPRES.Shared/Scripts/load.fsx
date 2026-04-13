// Bootstrap script for the GenPRES.Shared library.
// Loads all source files in dependency order so FSI scripts can use
// Shared types and calculations without requiring a prior dotnet build.

#I __SOURCE_DIRECTORY__

#load "../Types.fs"
#load "../Calculations.fs"

{
  lib,
  root ? ../.,
}:

let
  stripTrailingNewline = lib.removeSuffix "\n";

  gitMetadataPath = root + "/.git";

  readPath =
    path:
    if builtins.pathExists path then
      stripTrailingNewline (builtins.readFile path)
    else
      null;

  resolvePath =
    base: path:
    if lib.hasPrefix "/" path then
      /. + path
    else
      base + "/${path}";

  gitDir =
    if builtins.pathExists (gitMetadataPath + "/HEAD") then
      gitMetadataPath
    else if builtins.pathExists gitMetadataPath then
      let
        gitMetadata = readPath gitMetadataPath;
        gitDirMatch = builtins.match "gitdir: (.*)" gitMetadata;
      in
      if gitDirMatch == null then
        null
      else
        resolvePath root (builtins.elemAt gitDirMatch 0)
    else
      null;

  commonDir =
    if gitDir == null then
      null
    else
      let
        commonDirText = readPath (gitDir + "/commondir");
      in
      if commonDirText == null then
        gitDir
      else
        resolvePath gitDir commonDirText;

  gitHead =
    if gitDir == null then
      null
    else
      readPath (gitDir + "/HEAD");

  gitRevision =
    if gitHead == null then
      "unknown"
    else if lib.hasPrefix "ref: " gitHead then
      let
        gitRef = lib.removePrefix "ref: " gitHead;
        gitRefPath = commonDir + "/${gitRef}";
      in
      if commonDir != null && builtins.pathExists gitRefPath then
        readPath gitRefPath
      else
        "unknown"
    else
      gitHead;
in
if gitRevision == "unknown" then
  gitRevision
else
  builtins.substring 0 7 gitRevision
